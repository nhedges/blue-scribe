#include <stdio.h>
#include <string.h>
#include "pico/stdlib.h"
#include "hardware/gpio.h"
#include "hardware/pwm.h"
#include "hardware/uart.h"

// Hardware limits of the ball screw motors
#define XLIMIT 19000
#define YLIMIT 19000

#define RX_BUF_SIZE 128

#define X_MOTOR_STEP 18
#define X_MOTOR_DIR 19
#define Y_MOTOR_STEP 20
#define Y_MOTOR_DIR 21
#define X_MOTOR_RST 4
#define Y_MOTOR_RST 3

#define POWER_SAVE 0
#if POWER_SAVE
#define MOTOR_LOW_POWER 1
#else
#define MOTOR_LOW_POWER 0
#endif
#define MOTOR_ACTIVE_POWER 0

#define X_HOME_SWITCH 17
#define Y_HOME_SWITCH 16
/*#define LASER_POWER_CONTROL PICO_DEFAULT_LED_PIN*/
#define LASER_POWER_CONTROL 2

// UART hardware info
#define UART_ID uart0
#define BAUD_RATE 115200

#define LASER_IDLE 1

enum LaserState {idling, moving, homing};
enum IoState {recving, parsing, sending};
enum Function {GO, HM, SQ, NONE};
enum MotorStages {rising = 1, falling = 0};
static enum MotorStages xMotorStage = rising;
static enum MotorStages yMotorStage = rising;
volatile enum LaserState laserState = idling; //variable to keep track of the state we are in
volatile enum IoState ioState = recving; //variable to keep track of the state we are in
volatile enum Function function = NONE; //variable to keep track of the function that was originally called
static int motorTargetX = 0; //variable to keep track of our motor target
static int motorTargetY = 0; //variable to keep track of our motor target
static const int MOTOR_HZ = 400;
static repeating_timer_t motor_tick_timer; // Timer for motors updating


static uint8_t directionX = 0; //0 = CW, 1 = CCW
static uint8_t directionY = 0; //0 = CW, 1 = CCW
static int h = 0; //0 = horizontal stopped. 1 = horizontal moving
static int v = 0; //0 = vertical stopped. 1 = horizontal moving
static int motorLocationX = 0; //temp variable for current motor x position
static int motorLocationY = 0; //temp variable for current motor y position
static int homeX = 0; //variable for the home location
static int homeY = 0; //variable for the home location

const char doneFlag[5] = "A\n\r"; // what to print on successful op
const char errorFlag[9] = "Error\n\r"; // error message

static char serialRxBuf[RX_BUF_SIZE];
static unsigned int serialRxoffset = 0;
static char serialTxBuf[RX_BUF_SIZE];
static unsigned int txBufOffset = 0;

// initialize motor pins
void initMotorPins()
{
// 0 -3 
    // TODO figure out if we need to invert
        /*
        Example:
        // Our bitmap above has a bit set where we need an LED on, BUT, we are pulling low to light
        // so invert our output
        gpio_set_outover(gpio, GPIO_OVERRIDE_INVERT);
        */
    gpio_init(X_MOTOR_STEP);
    gpio_set_dir(X_MOTOR_STEP, GPIO_OUT);
    gpio_init(X_MOTOR_DIR);
    gpio_set_dir(X_MOTOR_DIR, GPIO_OUT);
    gpio_init(X_MOTOR_RST);
    gpio_set_dir(X_MOTOR_RST, GPIO_OUT);
    gpio_put(X_MOTOR_RST, MOTOR_LOW_POWER);

    gpio_init(Y_MOTOR_STEP);
    gpio_set_dir(Y_MOTOR_STEP, GPIO_OUT);
    gpio_init(Y_MOTOR_DIR);
    gpio_set_dir(Y_MOTOR_DIR, GPIO_OUT);
    gpio_init(Y_MOTOR_RST);
    gpio_set_dir(Y_MOTOR_RST, GPIO_OUT);
    gpio_put(Y_MOTOR_RST, MOTOR_ACTIVE_POWER);
}

// initialize home limit switch pins
void initHomeSwitchPins()
{
    gpio_init(X_HOME_SWITCH);
    gpio_set_dir(X_HOME_SWITCH, GPIO_IN);
    // We are using the button to pull down to 0v when pressed, so ensure that when
    // unpressed, it uses internal pull ups. Otherwise when unpressed, the input will
    // be floating.
    gpio_pull_up(X_HOME_SWITCH);
    gpio_init(Y_HOME_SWITCH);
    gpio_set_dir(Y_HOME_SWITCH, GPIO_IN);
    gpio_pull_up(Y_HOME_SWITCH);
}

// initialize PWM for laser
void initLaserPwm()
{
    // Tell the laser pin that the PWM is in charge of its value.
    gpio_set_function(LASER_POWER_CONTROL, GPIO_FUNC_PWM);
    // Figure out which slice we just connected to the laser pin
    uint slice_num = pwm_gpio_to_slice_num(LASER_POWER_CONTROL);
    // Get some sensible defaults for the slice configuration. By default, the
    // counter is allowed to wrap over its maximum range (0 to 2**16-1)
    pwm_config config = pwm_get_default_config();
    // Set divider, reduces counter clock to sysclock/this value
    // 6MHz / 20kHz = 300
    pwm_config_set_clkdiv(&config, 300.f);
    // Load the configuration into our PWM slice, and set it running.
    pwm_init(slice_num, &config, true);
    setLaser(LASER_IDLE);
}

// initialize ADC for laser thermometer
void initLaserAdc()
{
    //TODO
}

void setXdir(uint8_t direction)
{
    gpio_put(X_MOTOR_DIR, direction ? 0:1);
    directionX = direction;
}

void setYdir(uint8_t direction)
{
    gpio_put(Y_MOTOR_DIR, direction ? 0:1);
    directionY = direction;
}

bool step_x()
{
    if (xMotorStage == rising)
    {
        gpio_put(X_MOTOR_STEP, 1);
        xMotorStage = falling;
        return true;
    }
    else
    {
        gpio_put(X_MOTOR_STEP, 0);
        xMotorStage = rising;
        return false;
    }
}

bool step_y()
{
    if (yMotorStage == rising)
    {
        gpio_put(Y_MOTOR_STEP, 1);
        yMotorStage = falling;
        return true;
    }
    else
    {
        gpio_put(Y_MOTOR_STEP, 0);
        yMotorStage = rising;
        return false;
    }
}

void sendUartAck()
{
    memcpy(serialTxBuf, "A", 2);
    ioState = sending;
}

void setLaser(uint8_t duty)
{
    // shift 8-bit duty to 16-bit
    pwm_set_gpio_level(LASER_POWER_CONTROL, (duty << 8));
}

bool motor_tick_timer_callback(__unused repeating_timer_t *rt)
{
    switch(laserState)
    {
        case homing:
            setXdir(0);
            setYdir(0);
            bool xhome = !gpio_get(X_HOME_SWITCH);
            bool yhome = !gpio_get(Y_HOME_SWITCH);
            if (xhome && yhome)
            {
                laserState = idling;
                gpio_put(X_MOTOR_RST, MOTOR_LOW_POWER);
                gpio_put(Y_MOTOR_RST, MOTOR_LOW_POWER);
                motorLocationX = 0;
                motorLocationY = 0;
                sendUartAck();
                return false;
            }
            if (!xhome)
            {
                if (step_x())
                {
                    motorLocationX--; // TODO mockup only
                }
            }
            else
            {
                gpio_put(X_MOTOR_RST, MOTOR_LOW_POWER);
            }
            if (!yhome)
            {
                if(step_y())
                {
                    motorLocationY--; // TODO mockup only
                }
            }
            else
            {
                gpio_put(Y_MOTOR_RST, MOTOR_LOW_POWER);
            }
            break;
        case moving:
            // TODO add checks to homing switches for procection 
            if (motorTargetX == motorLocationX && motorTargetY == motorLocationY)
            {
                // destination point reached
                gpio_put(X_MOTOR_RST, MOTOR_LOW_POWER);
                gpio_put(Y_MOTOR_RST, MOTOR_LOW_POWER);
                laserState = idling;
                setLaser(LASER_IDLE);
                sendUartAck();
                return false;
            }
            if (motorTargetX > motorLocationX)
            {
                setXdir(1);
                if (step_x())
                {
                    motorLocationX++;
                }
            }
            else if (motorTargetX < motorLocationX)
            {
                setXdir(0);
                if(step_x())
                {
                    motorLocationX--;
                }
            }
            if (motorTargetY > motorLocationY)
            {
                setYdir(1);
                if(step_y())
                {
                    motorLocationY++;
                }
            }
            else if (motorTargetY < motorLocationY)
            {
                setYdir(0);
                if(step_y())
                {
                    motorLocationY--;
                }
            }
            break;
        default:
    }
    return true; // continue repeating
}

bool startHoming()
{
    // Don't care about current motor state
    // set power level PWM to zero
    setLaser(LASER_IDLE);
    // move into homing mode
    laserState = homing;
    gpio_put(X_MOTOR_RST, MOTOR_ACTIVE_POWER);
    gpio_put(Y_MOTOR_RST, MOTOR_ACTIVE_POWER);

    // negative timeout means exact delay (rather than delay between callbacks)
    cancel_repeating_timer(&motor_tick_timer);
    if (!add_repeating_timer_us(-1000000 / MOTOR_HZ, motor_tick_timer_callback, NULL, &motor_tick_timer))
    {
        printf("ERROR: Failed to add timer\n");
        return false;
    }
}

bool startGo(int xDest, int yDest)
{
    if (laserState == idling)
    {
        setLaser(LASER_IDLE);
        motorTargetX = xDest;
        motorTargetY = yDest;
        gpio_put(X_MOTOR_RST, MOTOR_ACTIVE_POWER);
        gpio_put(Y_MOTOR_RST, MOTOR_ACTIVE_POWER);
        laserState = moving;
        // start timer
        cancel_repeating_timer(&motor_tick_timer);
        if (!add_repeating_timer_us(-1000000 / MOTOR_HZ, motor_tick_timer_callback, NULL, &motor_tick_timer))
        {
            printf("ERROR: Failed to add timer\n");
            return false;
        }
        return true;
    }
    else
    {
        return false;
    }
}

bool startBurnHorizontal(int distance, int laserPower)
{
    if (laserState == idling)
    {
        motorTargetX = motorLocationX + distance; // TODO bounds check
        motorTargetY = motorLocationY;
        gpio_put(X_MOTOR_RST, MOTOR_ACTIVE_POWER);
        cancel_repeating_timer(&motor_tick_timer);
        bool timerResult = add_repeating_timer_us(-1000000 / MOTOR_HZ, motor_tick_timer_callback, NULL, &motor_tick_timer);
        if (timerResult)
        {
            laserState = moving;
            setLaser(laserPower);
            return true;
        }
        return false;
    }
    return false;
}

bool startBurnVertical(int distance, int laserPower)
{
    if (laserState == idling)
    {
        motorTargetX = motorLocationX;
        motorTargetY = motorLocationY + distance; // TODO bounds check
        gpio_put(Y_MOTOR_RST, MOTOR_ACTIVE_POWER);
        cancel_repeating_timer(&motor_tick_timer);
        bool timerResult = add_repeating_timer_us(-1000000 / MOTOR_HZ, motor_tick_timer_callback, NULL, &motor_tick_timer);
        if (timerResult)
        {
            laserState = moving;
            setLaser(laserPower);
            return true;
        }
        return false;
    }
    return false;
}

bool startBurnSquare(int size, int power)
{
    return false;
}

void chars_available_callback(void* ptr)
{
    (void)ptr;
    if (ioState != recving)
    {
        // protect the buffer from interrupt modification
        return;
    }
    int cRxedChar = getchar_timeout_us(0);
    if (cRxedChar == '\r')
    {
        serialRxBuf[serialRxoffset] = 0;
        memcpy(serialTxBuf, serialRxBuf, serialRxoffset);
        serialRxoffset = 0;
        ioState = parsing;
    }
    else if (serialRxoffset >= RX_BUF_SIZE-1)
    {
        memcpy(serialTxBuf, "ERROR: Command too long", 24);
        serialTxBuf[serialRxoffset] = 0;
        serialRxoffset = 0;
        ioState = sending;
    }
    else
    {
        serialRxBuf[serialRxoffset] = cRxedChar;
        serialRxoffset++;
    }
}

void coreLoop()
{
    switch(ioState)
    {
        case sending:
            printf("%s (%d, %d)\n", serialTxBuf, motorLocationX, motorLocationY);
            ioState = recving;
            break;
        case recving:
        break;
        case parsing:
            int param1, param2;
            if (2 == sscanf(serialRxBuf, "HM %d %d", &param1, &param2))
            {
                // HOME command matched
                startHoming();
            }
            else if (2 == sscanf(serialRxBuf, "GO %d %d", &param1, &param2))
            {
                // GO command matched
                if(!startGo(param1, param2))
                {
                    printf("ERROR: Not ready\n");
                }
            }
            else if (2 == sscanf(serialRxBuf, "BH %d %d", &param1, &param2))
            {
                if (!startBurnHorizontal(param1,  param2))
                {
                    printf("ERROR: Not ready\n");
                }
            }
            else if (2 == sscanf(serialRxBuf, "BV %d %d", &param1, &param2))
            {
                if (!startBurnVertical(param1,  param2))
                {
                    printf("ERROR: Not ready\n");
                }
            }
            else if (2 == sscanf(serialRxBuf, "SQ %d %d", &param1, &param2))
            {
                if (!startBurnSquare(param1,  param2))
                {
                    printf("ERROR: Not ready\n");
                }
            }
            else
            {
                printf("ERROR: Unknown command\n");
            }
            ioState = recving;
        break;
        default:
    }


}


int main()
{
    stdio_init_all();
    stdio_set_chars_available_callback(chars_available_callback, NULL);
    initMotorPins();
    initHomeSwitchPins();
    initLaserPwm();
    char buf[128];
    laserState = idling;
    ioState = recving;
    printf("Ready\n");
    while (1)
    {
        coreLoop();
    }
}

