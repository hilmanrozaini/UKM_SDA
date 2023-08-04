#### --- Audio Data Analysis --- #### 
#P119717 &  P121535 : Task 2

# loading the packages 
library(tuneR)
library(seewave)
library(audio)
library(ggplot2)
library(dplyr)

#---------------------Load the audio data -------------------------#

#importing the audio.wav  data

#1) ambulance
ambulance = "C:/Users/NUR MARDHIAH/Downloads/UKM/SEM 2/UNSTRUCTURED DATA ANALYTICS/PROJECT3/Audio/ambulance.wav"
data_ambulance <- tuneR::readWave(ambulance) %>%
  tuneR::normalize(unit = '16')

str(ambulance)

#2) firetruck

firetruck = "C:/Users/NUR MARDHIAH/Downloads/UKM/SEM 2/UNSTRUCTURED DATA ANALYTICS/PROJECT3/Audio/firetruck.wav"
data_firetruck <- tuneR::readWave(firetruck) %>%
  tuneR::normalize(unit = '16')

str(firetruck)

#3) traffic

traffic = "C:/Users/NUR MARDHIAH/Downloads/UKM/SEM 2/UNSTRUCTURED DATA ANALYTICS/PROJECT3/Audio/traffic.wav"
data_traffic <- tuneR::readWave(traffic) %>%
  tuneR::normalize(unit = '16')

str(traffic)

#-------------------waveform plot --------------------------#
# This provides an overview of the signal's loudness characteristics and allows
#for easy identification of patterns, peaks, and troughs.

# Extract the waveform data from the Wave object

### ambulance
waveform <- data_ambulance@left

# Create a time vector based on the sample rate and the length of the waveform
sample_rate <- data_ambulance@samp.rate
duration <- length(waveform) / sample_rate
time <- seq(0, duration, length.out = length(waveform))

# Combine time and waveform data into a data frame
data <- data.frame(Time = time, Amplitude = waveform)

# Create a waveform plot using ggplot2
ggplot(data, aes(x = Time, y = Amplitude)) +
  geom_line( colour= "lightblue") +
  labs(x = "Time (seconds)", y = "Amplitude", 
       title = "Amplitude vs Time for Ambulance Audio")


### firetruck
# Extract the waveform data from the Wave object
waveform1 <- data_firetruck@left

# Create a time vector based on the sample rate and the length of the waveform
sample_rate1 <- data_firetruck@samp.rate
duration1 <- length(waveform1) / sample_rate1
time2 <- seq(0, duration1, length.out = length(waveform1))

# Combine time and waveform data into a data frame
data1 <- data.frame(Time = time2, Amplitude = waveform1)

# Create a waveform plot using ggplot2
ggplot(data1, aes(x = Time, y = Amplitude)) +
  geom_line(colour= "orange") +
  labs(x = "Time (seconds)", y = "Amplitude", 
       title = "Amplitude vs Time for Firetruck Audio")

### traffic
# Extract the waveform data from the Wave object
waveform2 <- data_traffic@left

# Create a time vector based on the sample rate and the length of the waveform
sample_rate2 <- data_firetruck@samp.rate
duration2 <- length(waveform2) / sample_rate2
time3 <- seq(0, duration2, length.out = length(waveform2))

# Combine time and waveform data into a data frame
data2 <- data.frame(Time = time3, Amplitude = waveform2)

# Create a waveform plot using ggplot2
ggplot(data2, aes(x = Time, y = Amplitude)) +
  geom_line(colour= "lightgreen") +
  labs(x = "Time (seconds)", y = "Amplitude", 
       title = "Amplitude vs Time for Traffic Audio")


#---------------------spectrogram------------------------#
# changes in frequency components as a function of time
#It provides insights into the spectral characteristics of the audio signal 
#and helps identify specific frequency patterns or variations.

#construct the spectrogram for each audio
dev.off() #reset the graphic layout
par(mfrow=c(1,1))
spectro(data_ambulance, flim = c(0, 3))
spectro(data_firetruck, flim = c(0, 3))
spectro(data_traffic, flim = c(0, 3))

#------------------wave signal-------------------------------#
#  allows you to access the individual data points and apply various algorithms 
# or techniques directly to the audio signal,

#different  to different dataset

#1) data_ambulance
data_ambulance1 <- sine(440, duration = 20000); data_ambulance1 #look noise function from ?sine #state the duration 
data_ambulance2 <- noise(kind='pink',duration = 20000) ; data_ambulance2 #Change to pink noise (correlated noise).
data_ambulance3 <- pulse(220, duration = 20000) ; data_ambulance3

par(mfrow=c(3,1))
#The first 1000 signal for ambulance

plot(data_ambulance1[1:1000]) #SINE 
plot(data_ambulance2[1:1000]) #PINK  NOISE 
plot(data_ambulance3[1:1000]) #PULSE

#2) data_firetruck
data_firetruck1 <- sine(440, duration = 20000); data_firetruck1 #look noise function from ?sine #state the duration 
data_firetruck2 <- noise(kind='pink',duration = 20000) ; data_firetruck2 #Change to pink noise (correlated noise).
data_firetruck3 <- pulse(220, duration = 20000) ; data_firetruck3

par(mfrow=c(3,1))
#The first 1000 signal for ambulance

plot(data_firetruck1[1:1000]) #SINE 
plot(data_firetruck2[1:1000]) #PINK  NOISE 
plot(data_firetruck3[1:1000]) #PULSE

#3) data_traffic
data_traffic1 <- sine(440, duration = 20000); data_traffic1 #look noise function from ?sine #state the duration 
data_traffic2 <- noise(kind='pink',duration = 20000) ; data_traffic2 #Change to pink noise (correlated noise).
data_traffic3 <- pulse(220, duration = 20000) ; data_traffic3

par(mfrow=c(3,1))
#The first 1000 signal for ambulance

plot(data_traffic1[1:1000]) #SINE 
plot(data_traffic2[1:1000]) #PINK  NOISE 
plot(data_traffic3[1:1000]) #PULSE

#----------------------------FFT function ----------------------------------#

#
#Construct the left channel audio signal u
data_ambulance@left[1:50] # This returns the values of the waveform
Yk_amb <- fft(data_ambulance@left)
head(Yk_amb)

data_firetruck@left[1:50] # This returns the values of the waveform
Yk_fi <- fft(data_firetruck@left)
head(Yk_fi)

data_traffic@left[1:50] # This returns the values of the waveform
Yk_tr <- fft(data_traffic@left)
head(Yk_tr)


#plot the first 1000 spectogram frequency on the left channel
#only look at the highest frequency 
par(mfrow=c(1,1))  
plot.frequency.spectrum(Yk_amb[1:5000]) #first 5000 frequency spectrum , 4200+++

par(mfrow=c(1,2))  
plot.frequency.spectrum(Yk_fi[1:5000]) #first 5000 frequency spectrum
plot.frequency.spectrum(Yk_fi[1:3000]) #first 3000 frequency spectrum and the highest is close to 3000

plot.frequency.spectrum(Yk_tr[1:5000]) #first 5000 frequency spectrum 
plot.frequency.spectrum(Yk_tr[1:1000]) #first 5000 frequency spectrum and roughly 150++



