#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#include "blink.h"

void setLED(uint8_t value)
{
    PORTB = (!!value) << PINB5; /* PB5 on the atmega168 */
}

/* This should be called 1000 times/second. */
ISR(TIMER0_OVF_vect) {
    blink_atom();
    TCNT0 = 5;
}

void setup()
{
    TCCR0B |= _BV(CS01) | _BV(CS00);
    TIMSK0 = _BV(TOIE0);

    DDRB = 0xFF;
    TCNT0 = 5;

    sei();
}

int main(int argc, char * argv[])
{
    setup();

    while(1)
    { ; }
}
