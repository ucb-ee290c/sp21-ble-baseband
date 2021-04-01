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
dataI = [15, 18, 20, 8, 2, 0, 3, 11, 21, 28, 30, 27, 19, 10, 2, 0, 2, 10, 19, 27, 30, 28, 21, 11, 3, 0, 2, 9, 18, 26, 30, 29, 22, 13, 4, 0, 1, 7, 17, 25, 30, 29, 23, 14, 5, 0, 0, 6, 15, 24, 29, 30, 25, 16, 7, 1, 0, 4, 12, 21, 28, 30, 28, 20, 11, 4, 0, 1, 7, 16, 24, 29, 30, 26, 17, 9, 2, 0, 2, 9, 18, 26, 30, 29, 24, 16, 7, 1, 0, 3, 11, 19, 27, 30, 29, 23, 13, 6, 0, 0, 5, 12, 22, 28, 30, 28, 21, 12, 4, 0, 1, 6, 15, 23, 29, 30, 26, 19, 10, 3, 0, 1, 8, 17, 25, 30, 30, 25, 16, 8, 1, 0, 3, 11, 20, 28, 30, 28, 21, 11, 3, 0, 2, 9, 19, 27, 30, 28, 20, 11, 3, 0, 3, 10, 21, 28, 30, 26, 17, 8, 1, 0, 5, 14, 24, 30, 30, 24, 14, 5, 0, 1, 8, 17, 26, 30, 29, 22, 12, 4, 0, 1, 8, 17, 25, 30, 29, 24, 15, 6, 1, 0, 4, 12, 21, 28, 31, 27, 21, 12, 4, 0, 1]

dataQ = [15, 11, 0, 2, 8, 17, 25, 30, 29, 24, 15, 6, 1, 1, 7, 15, 24, 30, 30, 25, 16, 7, 1, 1, 6, 14, 23, 29, 30, 26, 17, 8, 2, 1, 5, 13, 22, 29, 30, 27, 19, 10, 3, 0, 4, 11, 20, 28, 31, 28, 21, 12, 4, 0, 2, 9, 18, 26, 30, 29, 24, 15, 7, 1, 1, 5, 13, 22, 28, 30, 28, 21, 12, 5, 0, 2, 7, 15, 24, 29, 30, 26, 18, 10, 3, 0, 3, 9, 18, 25, 30, 30, 25, 17, 8, 2, 0, 3, 11, 19, 27, 30, 29, 24, 15, 7, 1, 1, 5, 12, 21, 28, 30, 28, 22, 13, 5, 1, 1, 6, 14, 23, 29, 30, 27, 20, 11, 4, 1, 2, 8, 17, 25, 30, 30, 24, 16, 7, 1, 1, 6, 14, 23, 29, 30, 25, 17, 8, 1, 1, 6, 15, 25, 30, 30, 24, 14, 5, 0, 2, 9, 18, 27, 30, 28, 20, 10, 3, 0, 4, 12, 22, 29, 30, 26, 18, 8, 2, 1, 5, 13, 22, 29, 30, 27, 19, 10, 3, 0, 3, 10, 19, 26, 30, 29, 23, 15, 7, 1, 1, 5, 13, 21]
ht = hilbert_transform(dataQ)
#plt.plot([i for i in range(len(dataI))], dataI)
#plt.plot([i for i in range(len(dataQ))], dataQ)
print([float(dataI[t] - ht[t]) for t in range(len(dataI))])
#plt.plot(t, I)
plt.show()
