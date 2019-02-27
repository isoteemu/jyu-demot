/**
 * Demo 5 Tehtävä 1.
 */

#include <stdio.h>
#include <wiringPi.h>

#define PIN_LED 16
#define PIN_PIR 24

int main (void)
{
    printf("Lukee liikesensoria Pin %i ja käynnistää ledin Pin %i\n", PIN_PIR, PIN_LED);
    wiringPiSetupGpio();
    pinMode(PIN_PIR, INPUT);
    pinMode(PIN_LED, OUTPUT);
    for (;;)
    {
        if(digitalRead(PIN_PIR)) {
            printf("Liikettä havaittu\n");
            digitalWrite(PIN_LED, HIGH);
        } else {
            printf("Liikettä *EI* havaittu\n");
            digitalWrite(PIN_LED, LOW);
        }

        delay(100);
    }
    return 0;
}

