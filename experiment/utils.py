import os
import csv
import time
import math
import numpy as np
import itertools
import random
import pickle
from scipy import stats
from psychopy import core

def get_dirs(simulate = False):
    if simulate:
        data_folder = "data_sim"
    else:
        data_folder = "data"
    
    dirs = {
            "csv": os.path.join(os.path.curdir, data_folder, "csv"),
            "pkl": os.path.join(os.path.curdir, data_folder, "pkl")
        }
    return dirs

# TODO check if the with open statment is too slow
class csv_writer:
    """Create an object to save data trial-by-trial. using writer = csv_writer() create a csv file with column names. Then using writer.write(dict) the csv a row will be added to the csv file.
    """
    def __init__(self, cond, subject='', folder=''):
        """Initialize the csv_writer object

        Args:
            cond (dict): A dictionary with all experiment conditions. Keys will be used as column names
            subject (str, optional): The subject number. Defaults to ''.
            folder (str, optional): The CSV folder. Defaults to ''.
        """
        filename = '{}_({}).csv'
        current_time = time.strftime('%Y-%m-%d_%H-%M-%S', time.localtime())
        subject_path = os.path.join(folder, str(subject))
        self.save_file = filename.format(subject_path, current_time)
        self.colnames = list(cond.keys())
        self._setup_file()
    def _setup_file(self):
        """Initialize the csv writer object creating the file and writing the header
        """
        with open(self.save_file, 'w', newline='') as file: # w for creating
            self.writer = csv.DictWriter(file, fieldnames = self.colnames)
            self.writer.writeheader()
    def write(self, trial):
        """Takes a dictionary and write the values on the csv file.

        Args:
            trial (dict): A dictionary that identify the current trial to write
        """
        with open(self.save_file, 'a', newline='') as file: # a for appending
            self.writer = csv.DictWriter(file, fieldnames = self.colnames)
            self.writer.writerow(trial)
        
def deg2cm(angle, distance):
    """
    Returns the size of a stimulus in cm given:
        :distance: ... to monitor in cm
        :angle: ... that stimulus extends as seen from the eye

    Use this function to verify whether your stimuli are the expected size.
    (there's an equivalent in psychopy.tools.monitorunittools.deg2cm)
    """
    return math.tan(math.radians(angle)) * distance  # trigonometry

def getActualFrameRate(frames=1000):
    from psychopy import visual, core
    """
    Measures the actual framerate of your monitor. It's not always as clean as
    you'd think. Prints various useful information.
        :frames: number of frames to do test on.
    """
    
    # Set stimuli up
    durations = []
    clock = core.Clock()
    win = visual.Window(color='pink')

    # Show a brief instruction / warning
    visual.TextStim(win, text='Now wait and \ndon\'t do anything', color='black').draw()
    win.flip()
    core.wait(1.5)

    # blank screen and synchronize clock to vertical blanks
    win.flip()
    clock.reset()

    # Run the test!
    for i in range(frames):
        win.flip()
        durations += [clock.getTime()]
        clock.reset()

    win.close()

    # Print summary
    print('average frame duration was', round(np.average(durations) * 1000, 3), 'ms (SD', round(np.std(durations), 5), ') ms')
    print('corresponding to a framerate of', round(1 / np.average(durations), 3), 'Hz')
    print('60 frames on your monitor takes', round(np.average(durations) * 60 * 1000, 3), 'ms')
    print('shortest duration was ', round(min(durations) * 1000, 3), 'ms and longest duration was ', round(max(durations) * 1000, 3), 'ms')

def is_picklable(obj):
    """Check if an object can be converted to pkl. Useful to filter a list of objects to save

    Args:
        obj (_type_): A general python variable

    Returns:
        Logical: True if the object can be converted to pkl
    """

    try:
        pickle.dumps(obj)
    except Exception:
        return False
    return True

def save_objects(folder, subject, dict_to_save):
    """Save a list of objects in pkl format.

    Args:
        folder (string): Folder where saving the pkl file
        subject (string): Subject numeber
        dict_to_save (dict): Dictionary with object to save
    """
    
    filename = "s" + str(subject) + "_" + "session.pkl"
    backup = os.path.join(folder, filename)
    # subset only good elements
    dict_to_save = {k:v for (k, v) in dict_to_save.items() if is_picklable(v)}
    
    with open(backup, "wb") as backup_file:
        pickle.dump(dict_to_save, backup_file)
        
def restore_objects(dict_to_restore):
    """Restore a pkl file in the python session

    Args:
        dict_to_restore (string): The pkl file to restore

    Returns:
        dict: A dictionary with restored objects
    """
        
    with open(dict_to_restore, "rb") as backup_file:
        restored_file = pickle.load(backup_file)
    return restored_file

def create_conditions(cond, ncatch = 10):
    
    """Cartesian product of all dictionary values creating a list of dictionaries as trials for the main experiment.

    Args:
        cond (dict): Dictionary with all conditions
        prop_catch (float, optional): The proportions of catch trials to include. Defaults to 2/3.

    Returns:
        _list_: _list of dictionaries with all_
    """
    tup_list = list(itertools.product(*cond.values()))
    trial_list = [{key:value for value, key in zip(tup, cond)} for tup in tup_list]
    
    valid_list = [d for d in trial_list if d["trial_type"] == "valid"] # subset valid
    catch_list = [d for d in trial_list if d["trial_type"] == "catch"] # subset catch

    idx = random.sample(range(len(valid_list)), ncatch) # random index
    catch_list = [catch_list[i] for i in idx] # subset list
    
    cue_side_catch =  ["left", "right"]*ncatch # create sequence of cue for catch
    # assing the quest to each catch
    for i in range(len(catch_list)):
        catch_list[i]["cue"] = cue_side_catch[i]

    return valid_list + catch_list, len(valid_list), ncatch # combine and return

def str2bool(v):
    """Convert a generic string to boolean. Thanks to https://stackoverflow.com/a/715468/9032257)

    Args:
        v (_string_): _A string to be converted_

    Returns:
        _boolean_: _The boolean value associated with the string_
    """
    return str(v).lower() in ("yes", "true", "t", "1")

"""
SIMULATION FUNCTIONS
"""

class simKeys:
    '''
    an object to simulate key presses
    
    keyList: a list of keys to watch
    name: randomly selected from keyList
    rtRange: [min RT, max RT] where min and max RT are sepecified in ms
    rt: randomly selected from rtRange
    thanks to Becca https://discourse.psychopy.org/t/auto-response-script-in-psychopy/19349
        
    '''
    def __init__(self, keyList, rtRange, obs):
        """Init the simKeys object

        Args:
            keyList (_list_): _list of strings that idenfity the available keys_
            rtRange (_tuple_): _tuple with the reaction time range_
            obs (_object_): _An observer object created with the psy_observer class_
        """
        keyList = [x for x in keyList if x != 'escape']
        if obs is not None:
            self.ri = obs.get_resp() # get response based on observer
            self.name = bin_to_pas(self.ri)
        else:
            self.name=np.random.choice(keyList)
        
        self.rt = np.random.choice(np.linspace(rtRange[0], rtRange[1])/1000)
        
class psy_observer:
    """_Create an ideal observer for a psychophysical task based on a psychometric function (cumulative normal)_
    """
    def __init__(self, threshold, slope, guess, lapses):
        """_Init the ideal observer_

        Args:
            threshold (_float_): _The threshold of the psychometric function_
            slope (_float_): _The slope of the psychometric function_
            guess (_type_): _The false alarm rate (lower bound)_
            lapses (_type_): _The lapse rate (upper bound)_
        """
        self.threshold = threshold
        self.slope = slope
        self.guess = guess
        self.lapses = lapses
    def get_resp(self, xi = None):
        """_Generate a PAS response as string. 1 = 0 and 2,3,4 = 1_

        Returns:
            _string_: _The PAS response for that trial_
        """
        if xi is not None:
            pi = self.guess + (1 - self.guess - self.lapses) * stats.norm.cdf(xi, self.threshold, self.slope)
        else:
            pi = self.guess + (1 - self.guess - self.lapses) * stats.norm.cdf(self.xi, self.threshold, self.slope)
        ri = np.random.binomial(1, pi, 1)[0] # get 01 according to
        return ri

def bin_to_pas(ri):
    if ri == 1:
        pas = np.random.choice([2,3,4])
    else:
        pas = 1
    return str(pas)

def create_project_structure(dir_dict):
    for key, value in dir_dict.items():
        make_dir_if(value)

def make_dir_if(path):
    if not os.path.exists(path):
        os.makedirs(path)
        
class listtext:
    HEADER = '\033[95m'
    OKBLUE = '\033[94m'
    OKCYAN = '\033[96m'
    OKGREEN = '\033[92m'
    WARNING = '\033[93m'
    FAIL = '\033[91m'
    ENDC = '\033[0m'
    BOLD = '\033[1m'
    UNDERLINE = '\033[4m',
    TICK = '\u2713'

def step_completed(msg):
    msgout = "{}{}{} {}".format(listtext.OKGREEN, listtext.TICK, listtext.ENDC, msg)
    print(msgout)
    
class triggerLog:
    def __init__(self):
        self.triggers_values = []
        self.triggers_names = []
    def add_trigger(self, trigger):
        self.triggers_values.append(trigger)
        
# class for storing the clock
class stimClock:
    def __init__(self):
        self.clock = core.Clock()
    def reset(self):
        self.clock.reset()
    def get_time(self):
        self.time = self.clock.getTime()
        
def key_from_value(dict, value):
    return list(dict.keys())[list(dict.values()).index(value)]

def dur_to_frames(dur, refresh_rate):
    # dur = duration in ms
    # refreash rate in hz
    return dur / (1 / refresh_rate * 1000)

def frames_to_dur(nframes, refresh_rate):
    # nframes = duration in ms
    # refreash_rate = monitor refresh rate in hz
    return 1/refresh_rate * nframes