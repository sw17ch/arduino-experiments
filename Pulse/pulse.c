#include <avr/io.h>
#include <avr/interrupt.h>
#include <util/delay.h>

#include "pulse.h"

/* #define HZ1000  HZ1000 */
#define HZ10000 HZ10000

#ifdef HZ1000
    #define SET_TCCR0B TCCR0B |= _BV(CS01) | _BV(CS00)
    #define SET_TCNT0 = 5;
#endif

#ifdef HZ10000
    #define SET_TCCR0B TCCR0B |= _BV(CS01)
    #define SET_TCNT0  TCNT0 = 155;
#endif

volatile uint8_t lock = 0;

void setLED(uint8_t value)
{
    PORTB = (!!value) << PB5;
}

/* This should be called 1000 times/second. */
ISR(TIMER0_OVF_vect) {
    if (!lock)
    {
        lock = 1;
        
        pulse_atom();
        SET_TCNT0;

        lock = 0;
    } 
    else
    {
        // OVERRUN, DISABLE INTERRUPTS
        cli();
        while (1) ;
    }
}

void setup()
{
    SET_TCCR0B;
    TIMSK0 = _BV(TOIE0);

    DDRB = 0xFF;
    SET_TCNT0;

    sei();
}

int main(int argc, char * argv[])
{
    setup();

    while(1)
    { ; }
}
