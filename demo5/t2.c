/**
 * Demo 5 Tehtävä 1. Lähes identtinen Demo 2:sen kanssa, lisänä signaalivalo
 * joka uupui toteutuksesta. Huomaa toteutuksessa diodin käyttö vain koska eh.
 */

#include <stdio.h>
#include <wiringPi.h>

#define PIN_LED 16
#define PIN_PAINIKE 19
#define PIN_PIR 24

#define PIN_AUTO_PUNAINEN 16
#define PIN_AUTO_KELTAINEN 20
#define PIN_AUTO_VIHREA 21
#define PIN_JALANKULKIJA_VIHREA 12
#define PIN_SIGNAALI 25

#define LIIKE_VIIVE 2 * 1000
#define KELTAINEN_VIIVE 1 * 1000
#define YLITYS_KESTO 3 * 1000

int main (void)
{
    printf("Demo 5 Tehtävä 2: Liikennevalot.\n");
    wiringPiSetupGpio();

    // Aseta alkutila
    pinMode(PIN_JALANKULKIJA_VIHREA, OUTPUT);
    pinMode(PIN_AUTO_PUNAINEN, OUTPUT);
    pinMode(PIN_AUTO_KELTAINEN, OUTPUT);
    pinMode(PIN_AUTO_VIHREA, OUTPUT);
    pinMode(PIN_SIGNAALI, OUTPUT);

    pinMode(PIN_PAINIKE, INPUT);
    pinMode(PIN_PIR, INPUT);

    digitalWrite(PIN_JALANKULKIJA_VIHREA, LOW);
    digitalWrite(PIN_AUTO_PUNAINEN, LOW);
    digitalWrite(PIN_AUTO_KELTAINEN, LOW);
    digitalWrite(PIN_SIGNAALI, LOW);
    digitalWrite(PIN_AUTO_VIHREA, HIGH);

    for (;;)
    {
        // Kun valot ovat vihreät autoilijoille ja painiketta on painettu, 
        // sytytetään signaalivalo (keltainen), joka kertoo jalankulkijalle, 
        // että painallus on rekisteröity. Valon tulee palaa napin
        // painamisesta siihen asti, kunnes jalankulkijoille vaihtuu vihreä valo.
        if(digitalRead(PIN_PAINIKE)) {
            printf("Painiketta painettu\n");
            digitalWrite(PIN_SIGNAALI, HIGH);

            // Kun nappia painetaan, tarkistetaan, näkyykö liikettä.
            // Jos ei näy liikettä, ei ole autoja, joten valot voi vaihtaa
            // jalankulkijoille suotuisiksi. Jos liikettä, odota hetki, jos
            // jono loppuisi. 
            if(digitalRead(PIN_PIR)) {
                printf("Liiketunnistimessa liikettä\n");
                delay(LIIKE_VIIVE);
            }

            digitalWrite(PIN_AUTO_KELTAINEN, HIGH);
            digitalWrite(PIN_AUTO_VIHREA, LOW);
            delay(KELTAINEN_VIIVE);

            printf("Päästetään jalankulkijat\n");

            digitalWrite(PIN_AUTO_KELTAINEN, LOW);
            digitalWrite(PIN_AUTO_PUNAINEN, HIGH);
            digitalWrite(PIN_AUTO_VIHREA, LOW);
            digitalWrite(PIN_SIGNAALI, LOW);
            digitalWrite(PIN_JALANKULKIJA_VIHREA, HIGH);
            delay(YLITYS_KESTO);

            printf("Audikuskit kuumenee.\n");
            digitalWrite(PIN_JALANKULKIJA_VIHREA, LOW);
            digitalWrite(PIN_AUTO_KELTAINEN, HIGH);
            delay(KELTAINEN_VIIVE);

            printf("Päästetään autot.\n");
            digitalWrite(PIN_AUTO_KELTAINEN, LOW);
            digitalWrite(PIN_AUTO_PUNAINEN, LOW);
            digitalWrite(PIN_AUTO_VIHREA, HIGH);
        } else {
            printf("Päästetään autot.\n");
        }
        delay(100);
    }
    return 0;
}

