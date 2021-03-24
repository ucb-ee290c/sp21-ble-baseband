import numpy as np
import matplotlib.pyplot as plt
from scipy.signal import butter, lfilter, freqz
import math

# Wave Constants
A = 1
h = 0.5 # modulation index
F = 3e6 # 3 MHz, target frequency
F_offset = 250000 # 250 kHz offset from modulation
wF = 2*np.pi*F # Angular frequency 
F_symbol = 1e6 # 1 MHz = 1/(1 us) where BLE symbol time is 1 us
T_symbol = 1e-6 # Symbol time of 1us in seconds
F_sample = 32e6# 16MHz, sampling frequency 
#F_sample = 400e6


# FIR Constants
bts = 0.5
oversampling_factor = 16 # FIR samples / symbol, must be a factor of F_cpu/F_symbol
symbol_span = 6  # 6 symbols covered in FIR 
sampling_interval = (F_sample/F_symbol)/oversampling_factor # Number of cycles between samples


def butter_lowpass(cutoff, fs, order=5):
    nyq = 0.5 * fs
    normal_cutoff = cutoff / nyq
    b, a = butter(order, normal_cutoff, btype='low', analog=False)
    return b, a

def butter_lowpass_filter(data, cutoff, fs, order=5):
    b, a = butter_lowpass(cutoff, fs, order=order)
    y = lfilter(b, a, data)
    return y

def butter_bandpass(bandstart, bandstop, fs, order=5):
	nyq = 0.5 * fs
	normal_bandstart = bandstart / nyq
	normal_bandstop = bandstop / nyq
	b, a = butter(order, [normal_bandstart, normal_bandstop], btype='bandpass', analog=False)
	return b,a

def butter_bandpass_filter(data, bandstart, bandstop, fs, order=5):
	b, a = butter_bandpass(bandstart, bandstop, fs, order=order)
	y = lfilter(b, a, data)
	return y

def frequency_plot(wave):
	wave_fft = np.fft.fft(wave)/len(wave)
	wave_fft = wave_fft[range(int(len(wave)/2))]

	frequencies = np.arange(int(len(wave)/2))/(len(wave)/F_sample)
	plt.plot(frequencies, abs(wave_fft))
	plt.show()
 

# we switch symbols every F_cpu/F_symbol clock cycles
#gaussian_samples = [-1*F_symbol for x in range(oversampling_factor*symbol_span+1)] #Initialize to the value of sending zeros
gaussian_samples = [0 for x in range(oversampling_factor*symbol_span+1)] #Initialize to the value to 0s

gaussian_weights = [1.40031165852587e-29, 1.96615531083535e-28, 2.61129112221425e-27, 3.28047792188801e-26, 3.89819268037825e-25, 4.38161121543532e-24, 4.65852824412610e-23, 4.68498259401012e-22, 
		   4.45668137609324e-21, 4.01014010052386e-20, 3.41312238223497e-19, 2.74782158914969e-18, 2.09251971005850e-17, 1.50728347886546e-16, 1.02698636084934e-15, 6.61879253298573e-15, 
		   4.03494125452166e-14, 2.32669801553591e-13, 1.26907467658144e-12, 6.54754802210512e-12, 3.19532150318902e-11, 1.47500939703734e-10, 6.44049678383832e-10, 2.66004083528932e-09, 
		   1.03920587945997e-08, 3.84024773840945e-08, 1.34233602092313e-07, 4.43820749225537e-07, 1.38802828277153e-06, 4.10613651611886e-06, 1.14898089755205e-05, 3.04114122166256e-05, 
		   7.61385712624428e-05, 0.000180308920607055, 0.000403900191900572, 0.000855805869858572, 0.00171522389312682, 0.00325170193996781, 0.00583102626813850, 0.00989062238233514, 
		   0.0158688918289140, 0.0240831856694898, 0.0345720890878145, 0.0469441703874624, 0.0602951068257410, 0.0732532377253947, 0.0841813597060414, 0.0915059645185963, 0.0940864809736566,
		   0.0915059645185963, 0.0841813597060414, 0.0732532377253947, 0.0602951068257410, 0.0469441703874624, 0.0345720890878145, 0.0240831856694898, 0.0158688918289140, 0.00989062238233514, 
		   0.00583102626813850, 0.00325170193996781, 0.00171522389312682, 0.000855805869858572, 0.000403900191900572, 0.000180308920607055, 7.61385712624428e-05, 3.04114122166256e-05, 
		   1.14898089755205e-05, 4.10613651611886e-06, 1.38802828277153e-06, 4.43820749225537e-07, 1.34233602092313e-07, 3.84024773840945e-08, 1.03920587945997e-08, 2.66004083528932e-09, 
		   6.44049678383832e-10, 1.47500939703734e-10, 3.19532150318902e-11, 6.54754802210512e-12, 1.26907467658144e-12, 2.32669801553591e-13, 4.03494125452166e-14, 6.61879253298573e-15, 
		   1.02698636084934e-15, 1.50728347886546e-16, 2.09251971005850e-17, 2.74782158914969e-18, 3.41312238223497e-19, 4.01014010052386e-20, 4.45668137609324e-21, 4.68498259401012e-22, 
		   4.65852824412610e-23, 4.38161121543532e-24, 3.89819268037825e-25, 3.28047792188801e-26, 2.61129112221425e-27, 1.96615531083535e-28, 1.40031165852587e-29]

def gaussian_fir(sample, cycle):
	if (cycle % sampling_interval == 0):
		gaussian_samples[1:len(gaussian_samples)] = gaussian_samples[0:len(gaussian_samples)-1]
		gaussian_samples[0] = sample
	return sum([sample * weight for (sample, weight) in zip(gaussian_samples, gaussian_weights)])

def coherent_demod(wave, cycle, data_wave):
	w1 = 2*np.pi*(F+F_offset)
	w0 = 2*np.pi*(F-F_offset)

	w1_cos_integral = []
	w1_cos_integral_tracker = 0

	w0_cos_integral = []
	w0_cos_integral_tracker = 0


	guess = []

	for i in range(cycle):
		w1_cos_integral_base = w1_cos_integral_tracker if i%(F_sample/F_symbol) != 0 else 0
		w1_cos_integral_tracker = wave[i] * np.cos(w1*i/F_sample) * 1/F_sample + w1_cos_integral_base
		w1_cos_integral.append(w1_cos_integral_tracker)
		
		w0_cos_integral_base = w0_cos_integral_tracker if i%(F_sample/F_symbol) != 0 else 0
		w0_cos_integral_tracker = wave[i] * np.cos(w0*i/F_sample) * 1/F_sample + w0_cos_integral_base
		w0_cos_integral.append(w0_cos_integral_tracker)

		if (i % (F_sample/F_symbol) == (F_sample/F_symbol) - 1):
			guess.append(1 if abs(w1_cos_integral_tracker) < abs(w0_cos_integral_tracker) else 0) # < if we initialize FIR to 0, > if we initialize FIR to the +/- 1*T_s value 

	print(data[0:len(data)-3])
	print(np.asarray(guess[3:])) 

	plt.plot(range(cycle), [abs(x) for x in w0_cos_integral], color='red')
	plt.plot(range(cycle), [abs(x) for x in w1_cos_integral], color='blue')
	plt.show()

	return guess

def envelope_detector(wave):
	double_square_wave = [2*(x**2) for x in wave]
	low_pass_1 = butter_lowpass_filter(double_square_wave, 3e6, F_sample)
	envelope = [np.sqrt(x) for x in low_pass_1]
	return envelope

def noncoherent_demod(wave, cycle, data_wave):
	w0_bandpass = butter_bandpass_filter(wave, F-1.95*F_offset, F-0.05*F_offset, F_sample)
	w0_envelope = envelope_detector(w0_bandpass)

	w1_bandpass = butter_bandpass_filter(wave, F+0.05*F_offset, F+1.95*F_offset, F_sample)
	w1_envelope = envelope_detector(w1_bandpass)

	experimental_bandpass = butter_bandpass_filter(wave, F, F+2*F_offset, F_sample, order=5)
	plt.plot(range(cycle), experimental_bandpass, color='black')
	plt.show()

	guess = []
	demod_wave = []
	tracking_sum = 0

	for i in range(cycle):
		tracking_sum = tracking_sum + (w1_envelope[i] - w0_envelope[i])
		demod_wave.append(1 if w1_envelope[i] > w0_envelope[i] else -1)

		if (i % (F_sample/F_symbol) == (F_sample/F_symbol) - 1):
			guess.append(1 if tracking_sum > 0 else 0) 
			tracking_sum = 0


	#plt.plot(range(cycle), wave, color='black')
	plt.plot(range(cycle), w0_bandpass, color='b')
	plt.plot(range(cycle), w1_bandpass, color='r')
	#plt.show()

	plt.plot(range(cycle), w0_envelope, color='cyan')
	plt.plot(range(cycle), w1_envelope, color='orange')

	plt.plot(range(cycle), data_wave, color='black')
	plt.plot(range(cycle), demod_wave, color='green')

	plt.show()

	print(np.asarray(guess[3:]))

def modulate(data):
	data_idx = 0
	cycle = 0 
	s_t = 0
	phi_t = 0
	wave = []
	data_wave = []
	data_fir_wave = []

	# time t = cycle count / clock frequency 

	#s(t) = A * cos(wF*t + phi(t))
	#phi(t) = h*np.pi*integral of sum of impulse contributions


	# Operating from the perspective that each loop iteration is a clock cycle of the DAC
	while (data_idx < len(data)):
		val_i = data[data_idx]
		a_i = 1 if val_i else -1
		fir_results = gaussian_fir(a_i*F_symbol, cycle)
		data_fir_wave.append(fir_results/F_symbol)
		phi_t = (fir_results * (1/F_sample)) + phi_t #a_i * (1/T_symbol) * (1/F_cpu) # 1/F_cpu is delta T 
		s_t = A * np.cos(wF*(cycle/F_sample) + h*np.pi*phi_t)
		wave.append(s_t)
		data_wave.append(val_i)
		cycle = cycle + 1
		data_idx = math.floor(cycle / (F_sample / F_symbol)) #queue equivalent

	plt.plot(range(cycle), wave, color='black')
	plt.plot(range(cycle), data_wave, color='blue')
	plt.plot(range(cycle), data_fir_wave, color='purple')
	plt.show()

	return wave, cycle, data_fir_wave

# DATA
data = np.asarray(np.random.randint(2, size=20))
#data = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
#data = [1, 1, 0, 0, 0, 1, 1, 1, 0,1]

#(wave, cycle, data_wave) = modulate(data)
#coherent_demod(wave, cycle, data_wave)
#noncoherent_demod(wave, cycle, data_wave)
modulate(data)
