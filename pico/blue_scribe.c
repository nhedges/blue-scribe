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

#define X_MOTOR_STEP 0
#define X_MOTOR_DIR 1
#define Y_MOTOR_STEP 2
#define Y_MOTOR_DIR 3

#define X_HOME_SWITCH 4
#define Y_HOME_SWITCH 5
#define LASER_POWER_CONTROL PICO_DEFAULT_LED_PIN

// UART hardware info
#define UART_ID uart0
#define BAUD_RATE 115200

enum State {idling, moving, homing, recving, sending};
enum Function {GO, HM, SQ, NONE};
enum MotorStages {rising = 1, falling = 0};
static enum MotorStages xMotorStage = rising;
static enum MotorStages yMotorStage = rising;
volatile enum State state = 0; //variable to keep track of the state we are in
volatile enum Function function = NONE; //variable to keep track of the function that was originally called
static int motorTargetX = 0; //variable to keep track of our motor target
static int motorTargetY = 0; //variable to keep track of our motor target
static const uint32_t motorSpeed = 62353; //Predefined value to feed into our systick reload register to get our motor to move slow TODO adjust for new HW


static uint8_t directionX = 0; //0 = CW, 1 = CCW
static uint8_t directionY = 0; //0 = CW, 1 = CCW
static int h = 0; //0 = horizontal stopped. 1 = horizontal moving
static int v = 0; //0 = vertical stopped. 1 = horizontal moving
static int motorLocationX = 0; //temp variable for current motor x position
static int motorLocationY = 0; //temp variable for current motor y position
static int homeX = 0; //variable for the home location
static int homeY = 0; //variable for the home location

#define UART_BUFFER_LEN 128 // input buffer size
static uint32_t uartBufIdx = 0; // tracks the buffer location
static char uartBuf[UART_BUFFER_LEN];
const char doneFlag[5] = "A\n\r"; // what to print on successful op
const char errorFlag[9] = "Error\n\r"; // error message

static int chars_rxed = 0;

static char serialTxBuf[RX_BUF_SIZE];

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
    gpio_init(Y_MOTOR_STEP);
    gpio_set_dir(Y_MOTOR_STEP, GPIO_OUT);
    gpio_init(Y_MOTOR_DIR);
    gpio_set_dir(Y_MOTOR_DIR, GPIO_OUT);
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
    pwm_set_gpio_level(PICO_DEFAULT_LED_PIN, 0);
}

// initialize ADC for laser thermometer
void initLaserAdc()
{
    //TODO
}


void chars_available_callback(void* ptr)
{
    (void)ptr;
    static char serialRxBuf[RX_BUF_SIZE];
    static unsigned int offset = 0;
    if (state != recving)
    {
        return;
    }
    int cRxedChar = getchar_timeout_us(0);
    if (cRxedChar == '\r')
    {
        memcpy(serialTxBuf, serialRxBuf, offset);
        serialTxBuf[offset] = 0;
        offset = 0;
        state = sending;
    }
    else if (offset >= RX_BUF_SIZE-1)
    {
        memcpy(serialTxBuf, "ERROR: Command too long", 24);
        serialTxBuf[offset] = 0;
        offset = 0;
        state = sending;
    }
    else
    {
        serialRxBuf[offset] = cRxedChar;
        offset++;
    }
}

void coreLoop()
{
    switch(state)
    {
        case sending:
            printf("%s\n", serialTxBuf);
            state = recving;
            break;
        case recving:
        break;
        default:
    }


}


int main()
{
    stdio_init_all();
    stdio_set_chars_available_callback(chars_available_callback, NULL);
    // initMotorPins();
    // initHomeSwitchPins();
    // initLaserPwm();
    char buf[128];
    state = recving;
    printf("Ready\n");
    while (1)
    {
        coreLoop();
        // sleep_ms(10000);
        // scanf("%s\n", buf, 128);
        //fgets(buf, 128, stdin);
        //sleep_ms(1000);
        //printf("%s\n", buf, 128);
    }
}

