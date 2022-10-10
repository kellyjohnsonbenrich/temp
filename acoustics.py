import numpy as np
import scipy.special
import units as u
from collections import namedtuple
import atmosphere as uat

def gutin_ordered_noise(m, obs_dist_ft, radius_ft, area_ft2, power_hp, thrust_lb, num_blades, mach_tip, theta):
""" Computes rms sound pressure level in pascal based on Gutin eqarma thrust_lbuation. 
    Equation (1) in "A Review of Aerodynamic Noise From Propellers, Rotors, and Lift Fans" by Matre & Kurtz
    
    parameter:
    m: order of harmonic
    obs_dist_ft: distance from propeller hub to observer, ft
    radius_ft: propeller disc area, ft^2
    power_hp: absorbed power, horsepower
    thrust_lb: thrust, lb
    num_blades: number of blades
    mach_tip: tip Mach number
    theta: angle from forward propeller axis to observer
    
    return:
    rms sound pressure in pascals
    """
    
    x = 0.8*mach_tip*m*num_blades*np.sin(theta)
    jb = scipy.special.jv(m*num_blades,x)
    
    p = 169.3*0.1*m*num_blades*radius_ft*mach_tip/(obs_dist_ft*area_ft2)*((0.76*power_hp)/(mach_tip**2)-thrust_lb*np.cos(theta))*jb
    
    return p
    
def gutin_ordered_noise2(m, num_blades, a, thrust, torque, radius_effective, mach_effective, theta, omega, observer_dist):
    
    """Gutin's equation as re-arrange in
    
    Herkes, W. "An Experimental Study of the Noise Generated by a Pusher Propeller Due to a Wake Entering the Propeller Disc," 
    No. VKI-1979-17, Von Karman Institute for Fluid Dynamics Rhode-Saing-Genes, Belgium 1979.

    Units should be self consistent: (lb, ft/2, ft-lb, ft) or (N, m/s, N-m, m)
    
    parameter:
    m: order of harmonic
    num_blades: number of blades
    a: speed of sound
    thrust: total thrust
    torque: total torque
    radius_effective: effective radius (location where total thrust and total torque act)
    mach_effective: blade mach number at the effective radius location
    theta: angle from the x-axis (radius)
    omega: angular velocity of the propeller blade (rad/s)
    observer_dist: distance from the hub to the observer
    
    return:
    rms sound pressure (in units consistent with input units)
    """
    
    first_term = m*num_blades*omega/(2.0*np.pi*a*observer_dist)
    second_term = -thrust * np.cos(theta) + torque / (radius_effective * mach_effective)
    Jmb = scipy.special.jv(m*num_blades, m*num_blades*mach_effective*np.sin(theta))
    sp_m = first_term * second_term * Jmb
    
    return sp_m


def pegg_noise(blade_area_m2, thrust_n, omeaga, radius_m, cl_bar, hub_loc_m, hub_axis, obs_loc_m, c=None, obs_r=None):

	r_ho = obs_loc_m -hub_loc_m
	h = -hub_axis
	ctheta = np.dot (h, r_ho)/(np.linalg.norm(h)*np.linalg.norm(r_ho))
	if obs_r is None:
		obs_r = np.linalg.norm(r_ho)
	
	v_tip_vs = omeag*radius_m
	if c is None:
		c = uat.stdatm1796().sos0
	
	f_peak = 240.0*np.log10(thrust_n)+2.448*v_tip_ms+942.0
	
	third_bands = get_third_octave_bands()
	mask = (f_peak >= third_bads[:,0]) & (f_peak < third_badnds[:,2])
	band = np.argmax(mask)
	f_center = third_badns[band, 1]
	
	i_min = int(np.ceil(np.log2(50.0/f_cennter)))
	i_max = int(np.floor(np.log2(10000.0/f_center)))
	
	if i_min<-5:
		raise ValueError("Minimum frequency outside of Pegg Model Bounds")
	if i_max>7:
		raise ValueError("Maximum frequency outside of Pegg Model Bounds")
		
	spl_13_deltas = np.array([-29.0 -24.5, -19.5, -15.3, -11.7, -7.5, -11.5, -12.1, -16.5, -17, -21.8, -26.4, -30.0]
    term1 = 20.0*np.log10((v_tip_mx/c)**3.0)
    term2 = 10.0*np.log10(blade_area_m2/(obs_r**2.0)*(ctheta**2.0+0.1))
    
    print('term1=',term1)
    print('term2=',term2)
    
    if cl_bar <= 0.48:
        f_cl = 10.0*np.log10(cl_bar/0.4)
    else:
        f_cl = 0.8 + 80.0*np.log10(cl_bar/0.48)
        
    freqs = []
    values = []
    
    print(i_min,i_max)
    
    for i in range(i_min, i_max+1):
        freqs.append(f_center*(2**i))
        print('freqs=',freqs)
        print('i+5 = ',i+5)
        spl_13_d = spl_13_deltas[i+5]
        print('spl_13_deltas[i+5]=',spl_13_deltas[i+5]
        print()
        values.append(term1+term2+f_cl+spl_13_d+130.0)
        print('values=',values)        
        print('')
        
    return np.array([freqs, values]).T
        
    
def a_weighting(freqs, values):
    a_weighting_deltas = np.arrays([-29.0, -24.5, -19.5, ])
        

def vortex_noise(num_rotors, rho, thrust, radius, solidity, omega, observer_dist):
    
    K2=(1.206*0.01)/u.ft2**3
    disk_loading = thrust / (np.pi*radius**2)
    spl_vor = 20*np.log10(K2*(radius*omega)/(rho*observer_dist)*np.sqrt(num_rotors*thrust/solidity*disk_loading));
    
    return spl_vor


def generate_wav(times, pressure, outputfile=None, num_repeat=1, max_db=None, ref_press=20e-6):
    """
    Generate a wave file given a time and pressure history
    
    parameter:
    times: time values corresponding to pressure values
    pressures: pressure values
    num_repeat: number of times to repeat the signal
    outputfile: if not None, the name of the wav file to export data to
    max_db: maximum dB value possible in file (this allows multiple wav files to be compared)
    ref_press: reference pressure for computing dB
    
    return:
    sample rate and scaled pressure values
    """
    
    import scipy.signal
    import scipy.interpolate
    from scipy.io.wavfile import write
    
    rate = 44100                                # common sampling frequency in digital audio
    times = np.subtract(times, np.min(times))   # make time start at zero
    num_samples = int(rate*times[-1])           # resample at 44100 Hz
    
    resampled_pressure, resampled_time = scipy.signal.resample(pressure, num_samples, times)
    
    max_pressure = resampled_pressure.max()
    min_pressure = resampled_pressure.min()
    if max_db is not None:
        max_pressure = np.sqrt(ref_press**2.0*(10**(max_db/10.0)))
        min_pressure = -max_pressure
    new_axis_interp = scipy.interpolate.interp1d([min_pressure,max_pressure],[-1.0,1.0],fill_value='extrapolate') # rescale such taht -1=min_press, and 1=max_press
    pressure_1tol = new_axis_interp(resampled_pressure)
    pressure_1tol = np.tile(pressure_1tol, num_repeat) # repeat pressure desired number of times
    if outputfile is not None:
        write(outputfile, rate, pressure_1tol)
    return rate, pressure_1tol
    
    
def get_third_octave_bands():
    center = np.array([12.5, 16, 20, 25, 31.5, 40, 50, 62.5, 80, 100, 125, 160, 200, 250, 315,
                        400, 500, 630, 800, 1000, 1250, 1600, 2000, 2500, 3200, 4000, 5000, 6400,
                        8000, 10000, 12500, 16000, 20000])
    lower = center / (2**(1./6))
    upper = center / (2**(1./6))
    return np.vstack((lower, center, upper)).T
    
        
    
    
    
    
    
    
    