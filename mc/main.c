#include "stm32l476xx.h"
#include <stdio.h>

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
enum State {idling, moving, homing, recving, sending};
enum MotorStages {rising = 1, falling = 0};
static enum MotorStages xMotorStage = rising;
static enum MotorStages yMotorStage = rising;
volatile enum State state = 0; //variable to keep track of the state we are in
static int motorTargetX = 0; //variable to keep track of our motor target
static int motorTargetY = 0; //variable to keep track of our motor target
static const uint32_t motorSpeed = 62353; //Predefined value to feed into our systick reload register to get our motor to move slow
// const uint8_t motorHalfStepsX[] =  {0x40, 0x80, 0x4, 0x8};//{0x40, 0xc0, 0x80, 0x84, 0x4, 0xc, 0x8, 0x48}; //setting the correct bits to 1 for a motor half step
// const uint8_t motorHalfStepsX[] =  {0x40, 0x80, 0x4, 0x8};//{0x40, 0xc0, 0x80, 0x84, 0x4, 0xc, 0x8, 0x48}; //setting the correct bits to 1 for a motor half step
const uint32_t motorMaskX = 0xc;
const uint32_t motorMaskY = 0xc0;
static uint8_t directionX = 0; //0 = CW, 1 = CCW
static uint8_t directionY = 0; //0 = CW, 1 = CCW
static int h = 0; //0 = horizontal stopped. 1 = horizontal moving
static int v = 0; //0 = vertical stopped. 1 = horizontal moving
static int motorLocationX = 0; //temp variable for current motor x position
static int motorLocationY = 0; //temp variable for current motor y position
static uint8_t steppingx = 0;
static uint8_t steppingy = 0;
static int readyx = 0;
static int readyy = 0;
static int homeX = 0;
static int homeY = 0;

const int BufferSize = 128;
uint32_t Rx2_Counter = 0;
char buffer[BufferSize];
char doneflag[3] = "A\n";
char errorflag[7] = "Error\n";





//Initalize the UART
void USART_Init(USART_TypeDef * USARTx){
	
	USARTx->CR1 &= ~USART_CR1_UE; //Disable the UART
	
	USARTx->CR1 &= ~USART_CR1_M; //Set data length to 8 bits
	
	USARTx->CR2 &= ~USART_CR2_STOP; //Select 1 stop bit
	
	USARTx->CR1 &= ~USART_CR1_PCE; //Set no parity
	
	USARTx->CR1 &= ~USART_CR1_OVER8;  //Set oversampling by 16
	
	USARTx->BRR = 0x683; //Set BAUD rate
	
	USARTx->CR1 |= (USART_CR1_TE | USART_CR1_RE); // Enable transmission and reception
	
	USARTx->CR1 |= USART_CR1_UE; //Enable USART
	
	while ((USARTx->ISR & USART_ISR_TEACK) == 0); //Verify that usart is ready fro transmission
	
	while ((USARTx->ISR & USART_ISR_REACK) == 0); //Verify that usart is ready for reception.
	
}

//Initiialize the required pins
void pininit(){

	//2 pins for home switches . input mode pull down resistor then logic low when switch is closed. e10 e11
	
	//Setting up the motor step and direction pins for both the horizontal and vertical motors

	GPIOB->MODER &= ~( GPIO_MODER_MODE2_1 | GPIO_MODER_MODE3_1 | GPIO_MODER_MODE6_1 | GPIO_MODER_MODE7_1); //Setting the GPIOB mode registers to input mode for pins 2,3,6,7

	GPIOB->MODER |= ( GPIO_MODER_MODE2_0 | GPIO_MODER_MODE3_0 | GPIO_MODER_MODE6_0 | GPIO_MODER_MODE7_0); //Seeting the GPIOB mode registers to output mode for pins 2,3,6,7 by orring a 1 into the 0 bit of the mode

	GPIOB->OTYPER &= ~(GPIO_OTYPER_OT2 | GPIO_OTYPER_OT3 | GPIO_OTYPER_OT6 | GPIO_OTYPER_OT7); //Seeting the GPIOB output type registers to push pull for pins 2,3,6,7

	//setting up the input pins for the home switchs

	GPIOE->MODER &= ~(GPIO_MODER_MODE10 | GPIO_MODER_MODE11); //Set GPIOE pins 10,11 to input mode

	GPIOE->PUPDR &= ~(GPIO_PUPDR_PUPDR10 | GPIO_PUPDR_PUPDR11); //Set GPIOE pins 10,11 to pull up

	GPIOE->PUPDR |= (GPIO_PUPDR_PUPDR10_0 | GPIO_PUPDR_PUPDR11_0);

	//Setting up the pin for the center button to start and pause the laser

	GPIOA->MODER &= ~(GPIO_MODER_MODE0); //Set GPIOA pin 0 to input mode

	GPIOA->PUPDR &= ~(GPIO_PUPDR_PUPDR0); //Set GPIOA pin 0 to pull down

	GPIOA->PUPDR |= (GPIO_PUPDR_PUPDR0_1); //Set GPIOA pin 0 to pull down
	
	//Setting up the PWM and TIM1 pins

	GPIOE->MODER &= ~(GPIO_MODER_MODE8); //Seeting the GPIO mode register to alternate function mode for pin 8

	GPIOE->MODER |= (GPIO_MODER_MODE8_1); //Setting the GPIOE mode register to alternate function mode for pin 8

	GPIOE->AFR[1] &= ~(0xF); //Selecting AF mode 1

	GPIOE->AFR[1] |= 0x1; //Selecting AF mode 1

	GPIOE->OSPEEDR &= ~(GPIO_OSPEEDER_OSPEEDR8); //Seeting output speed 

	GPIOE->PUPDR &= ~(GPIO_PUPDR_PUPD8); //Selecting no pull up or pull down
	
	//Setting up the UART2 pins
	GPIOD->MODER &= ~(GPIO_MODER_MODE5 | GPIO_MODER_MODE6); //Clear mode bits
	GPIOD->MODER |= (GPIO_MODER_MODE5_1 | GPIO_MODER_MODE6_1);	//Set pins PD5 and PD6 to AFmode
	
	GPIOD->AFR[0] |= 0x77 << (4*5); //Set pins 5 and 6 to AF7
	
	GPIOD->OSPEEDR |= 0xF << (2*5); // Set GPIO to high speed
	
	GPIOD->PUPDR &= ~(0xF << (2*5));
	GPIOD->PUPDR |= (0x5 << (2*5)); //Select pull-up
	
	GPIOD->OTYPER &= ~(0x3 << 5); //Set to push pull mode
	
	RCC->APB1ENR1 |= RCC_APB1ENR1_USART2EN; //Enable UART 2 Clock
	
	RCC->CCIPR &= ~(RCC_CCIPR_USART2SEL);
	RCC->CCIPR |= (RCC_CCIPR_USART2SEL_0);//Select system clock as usart clock source
	
	USART2->CR1 |= USART_CR1_RXNEIE; // Recieve register not empty interupt
	USART2->CR1 &= ~(USART_CR1_TXEIE); //Transmit register empty interrupt
	
}

//Initiialize timer 1 and pwm
void timer1init(){

	RCC->APB2ENR |= RCC_APB2ENR_TIM1EN; // Enable the timer 1 clock
	TIM1->CR1 &= ~TIM_CR1_DIR; //Select up counting
	TIM1->PSC = 11; // Prescaler of 11 yields 1.3kHz PWM. Smaller PSC -> higher frequency
	TIM1->ARR = 999; //Set PWM period change this value
	TIM1->CCMR1 &= ~TIM_CCMR1_OC1M; //Clear output compare mode bits for channel 1
	TIM1->CCMR1 |= TIM_CCMR1_OC1M; //Select PWM mode 2 oupt on Channel 1
	TIM1->CCMR1 |= TIM_CCMR1_OC1PE; //Output 1 preload enable
	TIM1->CCER &= ~TIM_CCER_CC1NP; //Select output polarity
	TIM1->CCER |= TIM_CCER_CC1NE; //Enable complemtary output of channel 1 
	TIM1->BDTR |= TIM_BDTR_MOE; //Main output enable
	TIM1->CCR1 = 999; //initial duty cycle set to 0
	TIM1->CR1 |= TIM_CR1_CEN; //Enable TIM1

}

//Initiailize systick
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

void send(int message) {

	while (!(USART2->ISR & USART_ISR_TXE)); //Check RXNE event

	if (state == sending) {

		if (message == 1) {
			//send done flag
			for (int i = 0; i < 2; i++) {
				while (!(USART2->ISR & USART_ISR_TXE));
				USART2->TDR = doneflag[i];//send our done flag A\n
			}
			
		}
		else if (message == 2) {
			for (int i = 0; i < 6; i++) {
				while (!(USART2->ISR & USART_ISR_TXE));
				USART2->TDR = errorflag[i];//send our done flag A\n
			}

		}
		
		Rx2_Counter = 0;
		for (int i = 0; i < BufferSize; i++){
			buffer[i] = 0;
		}
		while (!(USART2->ISR & USART_ISR_TC));
		USART2->ICR |= USART_ICR_TCCF;


		state = idling;
	}




}

//Function to send the laser to a certain point
void goTo(int posX, int posY){
	motorTargetX = posX;
	motorTargetY = posY;
	if (motorTargetX > motorLocationX)
		directionX = 0; // move away from zero
	else
		directionX = 1; // move towards zero
	if (motorTargetY > motorLocationY)
		directionY = 0; // move away from zero
	else
		directionY = 1; // move towards zero
	state = moving;
	
	while (state == moving); // wait until move is done
	send(1);
}

void setPower(int powerLevel)
{
	// the power level is on a scale of 1 to 999
	TIM1->CCR1 = 999-powerLevel;
}

void home()
{
	motorLocationX = 0;
	motorLocationY = 0;
	homeX = 0;
	homeY = 0;
	goTo(300,300);
	state = homing;
	while (state == homing); // wait until we are done
	send(1);
}

//Test function to engrave a square at 50% power and medium speed
void sq(int sideLength){
	uint32_t beginX = motorLocationX;
	uint32_t beginY = motorLocationY;
  goTo(sideLength + beginX, beginY); // TODO change these to burn moves
	goTo(sideLength + beginX, sideLength + beginY);
	goTo(beginX, sideLength + beginY);
	goTo(beginX, beginY);
}
	
//Systick handler. Used for moving the motors. Still needs a lot of work
void SysTick_Handler(void)
{
	
	if (state == moving)
	{
		if (motorLocationX != motorTargetX)// update the x motor stuff
		{
		  uint32_t motorTempX = ((directionX << 3) | (xMotorStage << 2));//Gpiob pin 2 controls the stepping of the motor and pin 3 controls direction
			GPIOB->ODR &= ~motorMaskX;
			GPIOB->ODR |= motorTempX;
			if (xMotorStage == falling) // we update the location on falling edge
			{
				if (directionX == 1) // update the motor location since we moved
				{
					motorLocationX--;
				}
				else
				{
					motorLocationX++;
				}
				xMotorStage = rising;
			}
			else
			{
				xMotorStage = falling;
			}
			
			
		}
		if (motorLocationY != motorTargetY) // update the y motor stuff
		{
		  uint32_t motorTempY = ((directionY << 7) | (yMotorStage << 6));//Gpiob pin 6 controls the stepping of the motor and pin 7 controls direction
			GPIOB->ODR &= ~motorMaskY;
			GPIOB->ODR |= motorTempY;
			if (yMotorStage == falling) // we update the location on falling edge
			{
				if (directionY == 1) // update the motor location since we moved
				{
					motorLocationY--;
				}
				else
				{
					motorLocationY++;
				}
				yMotorStage = rising;
			}
			else
			{
				yMotorStage = falling;
			}
		}
		
		if (motorLocationX == motorTargetX && motorLocationY == motorTargetY)
		{
			
			state = sending; // change state if we are done moving.
			
		}
	}
	if (state == homing)
	{
		if (homeX == 0) homeX = 1;//(~GPIOE->IDR & (0x1 << 10));
		if (homeY == 0) homeY = 1;//(~GPIOE->IDR & (0x1 << 11));
		if (homeX == 0)
		{
			directionX = 1;
			uint32_t motorTempX = ((directionX << 3) | (xMotorStage << 2));//Gpiob pin 2 controls the stepping of the motor and pin 3 controls direction
			GPIOB->ODR &= ~motorMaskX;
			GPIOB->ODR |= motorTempX;

			if (xMotorStage == rising) // toggle motor stage
				xMotorStage = falling;
			else
				xMotorStage = rising;
		}
		else
		{ // hold still
			xMotorStage = falling;
			uint8_t motorTempX = xMotorStage << 2; // direction is zero and pin 2 for stepping
			GPIOB->ODR &= ~motorMaskX;
			GPIOB->ODR &= motorTempX;
		}
		
		if (homeY == 0)
		{
			directionY = 1;
			uint32_t motorTempY = ((directionY << 7) | (yMotorStage << 6));//Gpiob pin 6 controls the stepping of the motor and pin 7 controls direction
			GPIOB->ODR &= ~motorMaskY;
			GPIOB->ODR |= motorTempY;
			if (yMotorStage == rising) // toggle motor stage
				yMotorStage = falling;
			else
				yMotorStage = rising;
		}
		else
		{ // hold still
			yMotorStage = falling;
			uint8_t motorTempY = yMotorStage << 6; // direction is zero and pin 2 for stepping
			GPIOB->ODR &= ~motorMaskY;
			GPIOB->ODR &= motorTempY;
		}
		
		if (homeX != 0 && homeY != 0) // both switches have hit
		{
			motorLocationX = 0; // set current location to zero, zero
			motorLocationY = 0;
			state = sending; // change state
		}
	}
}


void receive(){
	
	if (Rx2_Counter < BufferSize && state == idling){
		while (!(USART2->ISR & USART_ISR_RXNE));	//Check RXNE event
		buffer[Rx2_Counter] = USART2->RDR; //Reading RDR clears the RXNE flag
	
		
		if (buffer[Rx2_Counter] == 0x0D || Rx2_Counter >= BufferSize){
			state = recving;
		}
		Rx2_Counter++;
	}
		
}

//UART handler. Still needs recieve and send functions. A lot of work needed on this bad boy too
void USART2_IRQHandler(void){
	
		receive();

}

//Initializes the last necessary things and lets the program run
int main(void){
	
	// Switch system clock to HSI
	RCC->CR |= RCC_CR_HSION;  // set clock control reg to use HSI clock
	while ((RCC->CR & RCC_CR_HSIRDY) == 0); // wait until HSI clock is ready
	RCC->CFGR &= ~(RCC_CFGR_SW); //Selecet HSI as system clock source
	RCC->CFGR |= RCC_CFGR_SW_HSI; //Select HSI as system clock source
	while ((RCC->CFGR & RCC_CFGR_SWS) == 0); //Wait until HSI is used as system clock source

	RCC->AHB2ENR |= (RCC_AHB2ENR_GPIOAEN | RCC_AHB2ENR_GPIOBEN | RCC_AHB2ENR_GPIODEN | RCC_AHB2ENR_GPIOEEN); //Enable clocks for GPIOA, GPIOB, GPIOD, and GPIOE
	
	pininit(); //Initiialize the pins
	timer1init(); //Initialize tim1
	SysTick_Initialize(20080); // reload value for 1ms->40160
	

	NVIC_SetPriority(USART2_IRQn, 0); //Set the highest urgency
	NVIC_EnableIRQ(USART2_IRQn); //Enable NVIC interrupt
	NVIC_EnableIRQ(EXTI0_IRQn); //Enable the center button interupt
	RCC->APB2ENR |= RCC_APB2ENR_SYSCFGEN; 
	SYSCFG->EXTICR[0] &= ~(SYSCFG_EXTICR1_EXTI0); //Set interupts to port A
	SYSCFG->EXTICR[0] |= (SYSCFG_EXTICR1_EXTI0_PA);//Set interupts to port A

	EXTI->RTSR1 |= EXTI_RTSR1_RT0; //eanble rising edge trigger
	EXTI->IMR1 |= EXTI_IMR1_IM0; //disable masks

	USART_Init(USART2); //Initialize the UART

	home(); //reset the laser position
	//setPower(500);

  //sq(1000);
  //goTo(5, 0);
	//goTo(5000, 5000);
	//goTo(0, 5000);
	//goTo(0,0);
	//setPower(10);
	while(1){
		if (state == recving){
			volatile char arg1;
			volatile char arg2;

			volatile int temp1;
			volatile int temp2;
			
			volatile int t = sscanf(buffer, "%c%c %d %d", &arg1, &arg2, &temp1, &temp2);

			if (arg1 == 'G' && arg2 == 'O') {
				goTo(temp1, temp2);
			}
			else if (arg1 == 'H' && arg2 == 'M') {
				home();
			}
			else if (arg1 == 'B' && arg2 == 'V') {
				setPower(temp2);
				if ((motorLocationY + temp1) > 0 && (motorLocationY + temp1) < 19000) {
					goTo(motorLocationX, motorLocationY + temp1);
				}
				else {
					state = sending;
					send(2);
				}
			}
			else if(arg1 == 'B' && arg2 == 'H') {
				setPower(temp2);
				if ((motorLocationX + temp1) > 0 && (motorLocationX + temp1) < 19000) {
					goTo(motorLocationX + temp1, motorLocationY);
				}
				else {
					state = sending;
					send(2);
				}
				 
			}
			else if(arg1 == 'S' && arg2 == 'Q') {
				setPower(temp2);
				sq(temp1);
			}
			else {
				state = sending;
				send(2);
			}
					
		}
	}


}


