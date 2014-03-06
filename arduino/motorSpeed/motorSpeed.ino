//Copyright (C) 2014 Ashwith Jerome Rego
//
//This program is free software: you can redistribute it and/or modify
//it under the terms of the GNU General Public License as published by
//the Free Software Foundation, either version 3 of the License, or
//(at your option) any later version.
//
//This program is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General Public License
//along with this program. If not, see <http://www.gnu.org/licenses/>.


//Calculates the speed of a motor.
//Hardware is a IR sensor which 
//faces a wheel with 8 holes.
//Speed is calculated by measuring
//time between consecutive holes.
//A 3-point average is used to 
//smooth out any errors.

int pwmPin = 3;     //Motor speed control
int encPin = 2;     //Input for encoder pulse
int speedPin = A0;  //Motor speed knob

volatile float motorSpeed[3] = {0,0,0};  //Set of values to be averaged
volatile char ptr = 0;          //pointer to above array
volatile float mSpeed = 0;               //3 - point average Speed
volatile long prevTime = 0;  //Previous millisecond recording

void setup()
{
  pinMode(pwmPin, OUTPUT);
  pinMode(encPin, INPUT);
  pinMode(speedPin, INPUT);
  attachInterrupt(0, calcSpeed, RISING);
  
#ifdef DEBUG  
  Serial.begin(9600);
#endif
}

void loop()
{
  int speedSet = analogRead(speedPin);  //Read speed knob
  analogWrite(pwmPin, speedSet/4);      //Set the speed
}


//ISR for speed calculation
void calcSpeed()
{
  //Get current time stamp and find the difference
  //between this and the previous time stamp.
  long currTime = millis();
  int timeDiff = currTime - prevTime;

  //RPM = 1000*60/(timeDiff*8) = 7500/timeDiff
  float cSpeed= 7500.0/timeDiff;
  
  //Push current speed recording to the array
  //and calculate the three point average
  motorSpeed[ptr] = cSpeed;
  ptr = (ptr+1)%3;
  
  mSpeed =  (motorSpeed[0] + motorSpeed[1] + motorSpeed[2])/3.0;
  prevTime = currTime;
  
#ifdef DEBUG
  Serial.println(mSpeed);
#endif
}
