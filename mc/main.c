#include "stm32l476xx.h"


//*************************************  32L476GDISCOVERY ***************************************************************************
// STM32L4:  STM32L476VGT6 MCU = ARM Cortex-M4 + FPU + DSP, 
//           LQFP100, 1 MB of Flash, 128 KB of SRAM
//           Instruction cache = 32 lines of 4x64 bits (1KB)
//           Data cache = 8 lines of 4x64 bits (256 B)
//
// Joystick (MT-008A): 
//   Right = PA2        Up   = PA3         Center = PA0
//   Left  = PA1        Down = PA5
//
// User LEDs: 
//   LD4 Red   = PB2    LD5 Green = PE8
//   
// CS43L22 Audio DAC Stereo (I2C address 0x94):  
//   SAI1_MCK = PE2     SAI1_SD  = PE6    I2C1_SDA = PB7    Audio_RST = PE3    
//   SAI1_SCK = PE5     SAI1_FS  = PE4    I2C1_SCL = PB6                                           
//
// MP34DT01 Digital MEMS microphone 
//    Audio_CLK = PE9   Audio_DIN = PE7
//
// LSM303C eCompass (a 3D accelerometer and 3D magnetometer module): 
//   MEMS_SCK  = PD1    MAG_DRDY = PC2    XL_CS  = PE0             
//   MEMS_MOSI = PD4    MAG_CS  = PC0     XL_INT = PE1       
//                      MAG_INT = PC1 
//
// L3GD20 Gyro (three-axis digital output): 
//   MEMS_SCK  = PD1    GYRO_CS   = PD7
//   MEMS_MOSI = PD4    GYRO_INT1 = PD2
//   MEMS_MISO = PD3    GYRO_INT2 = PB8
//
// ST-Link V2 (Virtual com port, Mass Storage, Debug port): 
//   USART_TX = PD5     SWCLK = PA14      MFX_USART3_TX   MCO
//   USART_RX = PD6     SWDIO = PA13      MFX_USART3_RX   NRST
//   PB3 = 3V3_REG_ON   SWO = PB5      
//
// Quad SPI Flash Memory (128 Mbit)
//   QSPI_CS  = PE11    QSPI_D0 = PE12    QSPI_D2 = PE14
//   QSPI_CLK = PE10    QSPI_D1 = PE13    QSPI_D3 = PE15
//
// LCD (24 segments, 4 commons, multiplexed 1/4 duty, 1/3 bias) on DIP28 connector
//   VLCD = PC3
//   COM0 = PA8     COM1  = PA9      COM2  = PA10    COM3  = PB9
//   SEG0 = PA7     SEG6  = PD11     SEG12 = PB5     SEG18 = PD8
//   SEG1 = PC5     SEG7  = PD13     SEG13 = PC8     SEG19 = PB14
//   SEG2 = PB1     SEG8  = PD15     SEG14 = PC6     SEG20 = PB12
//   SEG3 = PB13    SEG9  = PC7      SEG15 = PD14    SEG21 = PB0
//   SEG4 = PB15    SEG10 = PA15     SEG16 = PD12    SEG22 = PC4
//   SEG5 = PD9     SEG11 = PB4      SEG17 = PD10    SEG23 = PA6
// 
// USB OTG
//   OTG_FS_PowerSwitchOn = PC9    OTG_FS_VBUS = PC11    OTG_FS_DM = PA11  
//   OTG_FS_OverCurrent   = PC10   OTG_FS_ID   = PC12    OTG_FS_DP = PA12  
//
// PC14 = OSC32_IN      PC15 = OSC32_OUT
// PH0  = OSC_IN        PH1  = OSC_OUT 
// 
// PA4  = DAC1_OUT1 (NLMFX0 WAKEUP)   PA5 = DAC1_OUT2 (Joy Down)
// PA3  = OPAMP1_VOUT (Joy Up)        PB0 = OPAMP2_VOUT (LCD SEG21)
//
//****************************************************************************************************************


static int debounce_wait;
static int ticks;
static int seconds;
static int minutes;
const char NumberChars[10] = {
        /* 0      1      2      3      4      5      6      7      8      9  */
        '0','1','2','3','4','5','6','7','8','9'
};

int row[4] = {10,11,12,13}; //creates an array we will use to put the correct bit in the odr
const char col_char[4][4] = {{'1','2','3','A'},{'4','5','6','B'},{'7','8','9','C'},{'*','0','%','D'}}; //set up a 2d array with all of the keypad values
int counter = 60; //sets up our counter variable
int state = 0; //variable to keep track of the state we are in
int motorTarget = 0; //variable to keep track of our motor target
const uint32_t motorFast = 4016; //Predefined value to feed into our systick reload register to get our motor to move fast
const uint32_t motorSlow = 623530; //Predefined value to feed into our systick reload register to get our motor to move slow
const uint8_t motorHalfSteps[] =  {0x40, 0x80, 0x4, 0x8};//{0x40, 0xc0, 0x80, 0x84, 0x4, 0xc, 0x8, 0x48}; //setting the correct bits to 1 for a motor half step
const int motorMask = 0xcc;
int direction = 0; //0 = stop, 1 = CW, -1 = CCW
int motorLocation = 0; //temp variable for current motor location
int digits = 0;
int speed = 1;

void SysTick_Initialize(uint32_t ticks)
{
	SysTick->CTRL = 0; // disable counter and IRQ
	SysTick->LOAD = ticks - 1; // set the load register

	//make systick least urgent
	NVIC_SetPriority(SysTick_IRQn, (1<<__NVIC_PRIO_BITS)-1);
	
	
	SysTick->VAL = 0; // reset counter value
	
	SysTick->CTRL |= SysTick_CTRL_CLKSOURCE_Msk; // 1 = processor clock 0=ext clk
	SysTick->CTRL |= SysTick_CTRL_TICKINT_Msk; // enable systick interrupt
	SysTick->CTRL |= SysTick_CTRL_ENABLE_Msk; // enable timer
}
void wait_short(){ //Our counter for a short wait used for keypad inputs
	int volatile count = 0x0;
	
	
	while (count < 0x2000){
		count++;
	}
}
	

void wait_long(){ //Our counter for a long wait used for keypad inputs
	int volatile count = 0x0;
	
	
	while (count < 0x40000){
		count++;
		int volatile j = count;
	}
}

void motor_move(uint8_t* s){ //our function that prints the char to the lcd if a button is pressed
	
	

		
	
	//Checks if the pound key gets pressed
	
		SysTick_Initialize(motorSlow); // reload value for fast wind up
		motorTarget = (counter % 60) * 69; //gets the correct final motor location
		direction = 1; //sets the direction to clock wise
		SysTick->CTRL |= SysTick_CTRL_ENABLE_Msk; //enables the systick timer
	
	
}

	
void SysTick_Handler(void)
{
	//LCD_DisplayString((uint8_t*)"Tick");
		if(debounce_wait > 0) debounce_wait--; // debounce
		else debounce_wait = 0;
	  //if(GPIOA->ODR & 0x1)
	  //GPIOA->ODR &= ~0x1;
		//else
			//GPIOA->ODR |= 0x1;
	
		//if(direction != 0){
		motorLocation += direction;
		uint8_t motorTemp = motorHalfSteps[motorLocation%4];
		GPIOB->ODR &= ~motorMask;
		GPIOB->ODR |= (motorMask & motorTemp);
			  	
		//}
		
		
}

int main(void){
	
	// Switch system clock to HSI
  RCC->CR |= RCC_CR_HSION;  // set clock control reg to use HSI clock
  while ((RCC->CR & RCC_CR_HSIRDY) == 0); // wait until HSI clock is ready
	
//	int was_pressed = 0;

//	
//	
	RCC->AHB2ENR |= RCC_AHB2ENR_GPIOBEN;// enable clocks on GPIO A,B,E 
//	//    (RCC_AHB2ENR_GPIOAEN |
//     RCC_AHB2ENR_GPIOEEN |
//	
//	
//	GPIOA->MODER &= ~( GPIO_MODER_MODE1_1 | 
//		GPIO_MODER_MODE2_1 |
//		GPIO_MODER_MODE3_1 |
//		GPIO_MODER_MODE5_1); //Setting the GPIOA mode registers to input mode for pins 1,2,3,5
//		
//	GPIOA->MODER &= ~( GPIO_MODER_MODE1_0 | 
//		GPIO_MODER_MODE2_0 |
//		GPIO_MODER_MODE3_0 |
//		GPIO_MODER_MODE5_0); //Setting the GPIOA mode registers to input mode for pins 1,2,3,5
//	
//	
//	GPIOA->MODER &= ~( GPIO_MODER_MODE0_1); //setting buzzer pin to output
//	GPIOA->MODER |= ( GPIO_MODER_MODE0_0);  //setting buzzer pin to output
//	GPIOA->OTYPER &= ~(GPIO_OTYPER_OT0); //setting buzzer pin to push pull
//	
//	GPIOE->MODER &= ~( GPIO_MODER_MODE10_1 | 
//		GPIO_MODER_MODE11_1 |
//		GPIO_MODER_MODE12_1 |
//		GPIO_MODER_MODE13_1); //Setting the GPIOE mode registers to 0 for pins 10,11,12,13
//	
//	
//	GPIOE->MODER |= ( GPIO_MODER_MODE10_0 | 
//		GPIO_MODER_MODE11_0 |
//		GPIO_MODER_MODE12_0 |
//		GPIO_MODER_MODE13_0);	//Seeting the GPIOE mode registers to output mode for pins 10,11,12,13 by orring a 1 into the 0 bit of the mode
//		
//		
//		GPIOE->OTYPER |= (GPIO_OTYPER_OT10 |
//			GPIO_OTYPER_OT11 |
//			GPIO_OTYPER_OT12 |
//			GPIO_OTYPER_OT13); //Seeting the GPIOE output type registers to open drain mode for pins 10,11,12,13
//			
//			
	 GPIOB->MODER &= ~( GPIO_MODER_MODE2_1 | 
	 GPIO_MODER_MODE3_1 |
	 GPIO_MODER_MODE6_1 |
		GPIO_MODER_MODE7_1);
 //Setting the GPIOB mode registers to input mode for pins 2,3,6,7
	
		GPIOB->MODER |= ( GPIO_MODER_MODE2_0 | 
		GPIO_MODER_MODE3_0 | 
		GPIO_MODER_MODE6_0 | 
		GPIO_MODER_MODE7_0);
//Seeting the GPIOB mode registers to output mode for pins 2,3,6,7 by orring a 1 into the 0 bit of the mode
		
		
		GPIOB->OTYPER &= ~(GPIO_OTYPER_OT2 |
		GPIO_OTYPER_OT3 |
		GPIO_OTYPER_OT6 |
			GPIO_OTYPER_OT7);
//Seeting the GPIOB output type registers to push pull for pins 2,3,6,7
	
	motor_move(speed);
	
	
	while(1);


}


