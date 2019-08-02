#!/usr/bin/python3

import numpy as np
import sys

def overflow():
    try:
        return np.int8(2**7 - 1) + np.int8(1)
    except FloatingPointError:
        print("Numeron ylivuoto, ja se havaittiin. ")



assert np.seterr()['over'] != 'ignore', "Numpyn ylivuoto ei ole oletus."

# ei virhettä
over = overflow()

assert over != np.int8(1 - 2**7), "Kahdeksanbittisen luvun olisi pitänyt vuotaa yli"

# aseta virhe, nostataa exceptionin
np.seterr(over='raise')
overflow()

assert True, "Sovelluksen olisi pitänyt kaatua jo, olen taitavempi kuin kuvittelinkaan."
