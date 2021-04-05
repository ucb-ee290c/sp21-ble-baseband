import matplotlib.pyplot as plt
import numpy as np
from fixedpoint import FixedPoint
from scipy.signal import butter, lfilter
from scipy import signal
from numpy import pi
from scipy.fft import fft, fftfreq, fftshift
import fixedpoint
import math

# Constants

MHz = lambda f: f * 1000000
GHz = lambda f: f * 1000000

channel_index = 0
F_RF = MHz(2402 + 2 * channel_index) # 2.402 GHz
F_IF = MHz(2)  # 2.5 MHz
F_LO = F_RF - F_IF # LO frequency is RF frequency - Intermediate Frequency
F_IM = F_LO - F_IF # Image is on the other side of the LO
analog_F_sample = (F_LO * 2 + F_IF) * 2
ADC_sample_rate = MHz(20)
t_interval = 0.00001

HB_coeff = [-0.0000,    0.0001,    0.0000,   -0.0009,   -0.0000,    0.0040,    0.0000,   -0.0128,   -0.0000, 0.0340,    0.0000,   -0.0850,   -0.0000,    0.3106,    0.5000,    0.3106,   -0.0000,   -0.0850, 0.0000,    0.0340,  -0.0000,   -0.0128,    0.0000,    0.0040,   -0.0000,   -0.0009,    0.0000, 0.0001,   -0.0000]
""" Method of obtaining Hilbert Transform FIR coefficients
https://www.wirelessinnovation.org/assets/Proceedings/2011/2011-1b-carrick.pdf
"""

HB_coeff = [2 * np.sin(i * pi / 2) * HB_coeff[i] for i in range(0, len(HB_coeff))]
#print(HB_coeff)

#HB_coeff = [0.0, 0.0, 0.0, 0.002, 0.0, 0.008, 0.0, 0.026, 0.0, 0.068, 0.0, 0.17, 0.0, 0.6212, 0.0, -0.6212, 0.0, -0.17, 0.0, -0.068, 0.0, -0.026, 0.0, -0.008, 0.0, -0.002, 0.0, 0.0, 0.0]

HB_coeff = [FixedPoint(c, True, 1, 11, str_base=2) for c in HB_coeff]
print(['b' + str(c) for c in HB_coeff])
def butter_lowpass(cutoff, fs, order=5):
    sos = signal.butter(10, cutoff, 'lp', fs=fs, output='sos')
    return sos

def butter_lowpass_filter(data, cutoff, fs, order=5):
    sos = butter_lowpass(cutoff, fs, order=order)
    y = signal.sosfilt(sos, data)
    return y

def frequency_plot(wave, F_sample):
    yf = fft(wave)
    xf = fftfreq(int(F_sample *t_interval), 1 / F_sample)
    print("X:",len(xf))
    xf = fftshift(xf)
    yplot = fftshift(yf)
    plt.plot(xf, 1.0/int(F_sample *t_interval) * abs(yplot))
    plt.grid()
    
def fir(signal):
    print(len(signal))
    elements = [0 for _ in range(len(HB_coeff) - 1)]
    elements.extend(signal)
    result = []
    for i in range(len(signal)):
        e = 0
        for j in range(len(HB_coeff)):
            e += HB_coeff[j] * elements[i + len(HB_coeff) - j - 1]
        result.append(e)
    return result[len(HB_coeff):]

def RF(t):
    return np.cos(2 * pi * (F_LO + F_IF + 0.) * t + pi / 4)
    
def IM(t):
    return np.cos(2 * pi * (F_LO - F_IF) * t + pi / 4)
    
def mix(signal):
    def I(t):
        return signal(t) * np.cos(2 * pi * F_LO * t)
    def Q(t):
        return signal(t) * np.sin(2 * pi * F_LO * t)
    return I, Q
    
def quantize(s, scale, range):
    return int((s - scale) / range * 31)#TODO
    
def ADC_sampling(sig, F_sample, OLD_F_sample):
    """
        Takes in signals `I` & `Q` sampled at `OLD_F_sample` and resamples them at a new sampling
    frequency `F_sample`.
    """
    sig_sampled = [quantize(s, min(sig), max(sig) - min(sig)) for s in sig[::int(OLD_F_sample//F_sample)]] # resample & quantize I
    num_samples = int(F_sample * t_interval) # determine the number of samples in the time interval
    max_valid_sample = min(num_samples, len(sig_sampled))
    results = np.linspace(0, t_interval, num_samples)[:max_valid_sample], sig_sampled[:max_valid_sample] # remove extraneous elements
    return results


def analog_lowpass(I, Q):
    return butter_lowpass_filter(I, F_IF + MHz(1), analog_F_sample), butter_lowpass_filter(Q, F_IF + MHz(1), analog_F_sample)
    
def hilbert_transform(Q):
    signal = Q
    elements = [0 for _ in range(len(HB_coeff))]
    elements.extend(signal)
    result = []
    for i in range(len(signal)):
        e = 0
        for j in range(len(HB_coeff)):
            e += HB_coeff[j] * elements[i + len(HB_coeff) - j - 1]
        result.append(e)
    return result

t = np.linspace(0, t_interval, num = int(analog_F_sample *t_interval))
I, Q = mix(lambda t: RF(t))
I, Q = I(t), Q(t)
I, Q = analog_lowpass(I, Q)
result = ADC_sampling(I, MHz(20), analog_F_sample)
print("i = ", result[1])
t = result[0]
I = [s - 15 for s in result[1]]
result = ADC_sampling(Q, MHz(20), analog_F_sample)
print("q = ", result[1])
Q = [s - 15 for s in result[1]]
I = [FixedPoint(s, True, 6, 0) for s in I]
Q = [FixedPoint(s, True, 6, 0) for s in Q]

data = [19, -3, -23, -28, -16, 6, 25, 28, 15, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -5, -22, -26, -14, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26, -16, 5, 23, 28, 17, -3, -22, -26]

#plt.plot(list(range(len(data))), data)
#plt.plot(t, ht)
#plt.plot(t, Q)
#plt.plot(t, [(I[t] - ht[t]).__float__() for t in range(len(t))])
data = [-25, -29, -25, -9, 9, 25, 31, 26, 12, -8, -23, -29, -25, -11, 6, 23, 31, 27, 14, -4, -21, -29, -27, -15, 4, 20, 30, 28, 17, -1, -19, -29, -29, -16, 1, 18, 30, 30, 19, 1, -17, -27, -29, -19, -1, 16, 28, 30, 21, 4, -14, -27, -29, -21, -5, 14, 27, 31, 23, 6, -11, -25, -29, -23, -8, 12, 26, 31, 25, 10, -9, -25, -29, -25, -9, 9, 25, 31, 26, 12, -7, -23, -30, -25, -13, 6, 22, 30, 28, 14, -3, -21, -30, -27, -15, 3, 20, 30, 28, 17, -1, -19, -28, -29, -18, 0, 19, 30, 29, 20, 2, -16, -27, -29, -19, -1, 16, 28, 31, 22, 4, -14, -27, -29, -21, -5, 14, 27, 31, 24, 6, -11, -25, -29, -23, -8, 11, 26, 32, 25, 10, -9, -24, -30, -25, -10, 9, 24, 30, 26, 12, -5, -23, -30, -26, -13, 6, 22, 30, 28, 15, -3, -21, -29, -27, -15, 2, 20, 30, 28, 18, -1, -18, -29, -29]
#ht = hilbert_transform([s[1] for s in data])
#plt.plot([i for i in range(len(data))], [s[0] for s in data])
#plt.plot([i for i in range(len(data))], [s[1] for s in data])
plt.plot([i for i in range(len(data))], data)
#plt.plot(t, I)
plt.show()
