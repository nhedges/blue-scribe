#include "pico/stdlib.h"
#include "hardware/gpio.h"
#include "hardware/pwm.h"
#include "hardware/uart.h"

// Hardware limits of the ball screw motors
#define XLIMIT 19000
#define YLIMIT 19000

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
// RX interrupt handler
void on_uart_rx() {
    while (uart_is_readable(UART_ID)) {
        uint8_t ch = uart_getc(UART_ID);
        // store into the buffer if there is room
        if (uartBufIdx < UART_BUFFER_LEN)
        {
            uartBuf[uartBufIdx] = ch;
            uartBufIdx++;
        }
        else
        {
            uart_puts(UART_ID, "Error: Command too long\n");
            uartBufIdx = 0;
        }
        // Can we send it back?
        /*
        if (uart_is_writable(UART_ID)) {
            // Change it slightly first!
            ch++;
            uart_putc(UART_ID, ch);
        }
        */
        chars_rxed++;
    }
}

// initialize the UART
void UartInit()
{
    uart_init(UART_ID, BAUD_RATE);

    // Set up a RX interrupt
    // We need to set up the handler first
    // Select correct interrupt for the UART we are using
    int UART_IRQ = UART_ID == uart0 ? UART0_IRQ : UART1_IRQ;

    // And set up and enable the interrupt handlers
    irq_set_exclusive_handler(UART_IRQ, on_uart_rx);
    irq_set_enabled(UART_IRQ, true);

    // Now enable the UART to send interrupts - RX only
    uart_set_irq_enables(UART_ID, true, false);

    // OK, all set up.
    // Lets send a basic string out, and then run a loop and wait for RX interrupts
    // The handler will count them, but also reflect the incoming data back with a slight change!
    uart_puts(UART_ID, "\nHello, uart interrupts\n");
}

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



int main()
{
    while (1)
    {
        tight_loop_contents();
    }
}

