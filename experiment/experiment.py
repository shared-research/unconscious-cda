# -*- coding: utf-8 -*-

"""
Modules
"""

from psychopy import core, visual, gui, monitors, event, data # psychopy stuff
from psychopy.parallel import ParallelPort
from triggers import triggers # importing the triggers dictionary
import numpy as np
import utils
import time
import random
import os

"""
Functions
"""

def sendTrigger(code, port, test = False, close = True):
    if test:
        print("Trigger " + str(code) + " sent")
        triglog.add_trigger(code)
        core.wait(0.004) # emulating the port closing
    else:
        port.setData(code)
        if close:
            core.wait(0.004) # delay for hardware opening-closing. 4ms for a 500hz EEG sampling rate
            port.setData(0) # resetting the port
            
def present(obj, nframes, trigger = None, clock = None):
    win.callOnFlip(clock.reset)
    win.callOnFlip(port.setData, trigger)
    for frame in nframes:
        obj.draw()
        win.flip()
    port.setData(0)
    dur = clock.get_time()
    
    return dur
    
            
def closePort(port, test = False):
    if not test:
        port.setData(0)
    
def get_test_angle(target, diff, type = None, which_change = None):
        if type == 'change':
            if which_change == "clock":
                res = target + diff
            else:
                if target < diff:
                    res = 360 - abs(target - diff)
                else:
                    res = target - diff
        else:
            res = target
        return res

def ask(nk = None, obj = None, msg = None, keyList=None, quitKey = 'escape', hold = False, simulate = False, rtRange = [200, 2000], obs = None, pos = (0,0)):
    """
    Display a msg and wait for keyboard response.
    Return the pressed key and the reaction time. Can append a before/after string. If hold = True no text is used. Useful for asking for a response while the stimulus is on the screen.
    """
    keyList_sim = keyList.copy() # keylist without escape for simulation
    keyList.append(quitKey)
    
    if not hold:
        obj.pos = pos
        obj.text = msg
        obj.draw()
        
    time_flip = win.flip()
    
    # select the first (only) response
    if simulate:
        keypress = utils.simKeys(keyList_sim, rtRange, obs)
        core.wait(keypress.rt)
        key_name = keypress.name
        key_rt = keypress.rt
    else:
        #key = kb.waitKeys(keyList = keyList, waitRelease = True)[0]
        key_name, key_rt = event.waitKeys(keyList=keyList, timeStamped=True)[0]

    return key_name, key_rt - time_flip


def feedback(pas_resp, resp_correct = "1"):
    """
    Give a feedback on catch trials for reducing false alarms. The default is 1 (not seen)
    """
    if pas_resp == resp_correct:
        FEEDBACK = FEEDBACK_POSITIVE.format(pas_resp)
    else:
        FEEDBACK = FEEDBACK_NEGATIVE.format(pas_resp)
    
    text.text = FEEDBACK
    text.draw()
    win.flip()
    core.wait(FEEDBACK_DUR)

def feedback_CDT(response):
    if response == 1:
        text.text = FEEDBACK_CORRECT
    else:
        text.text = FEEDBACK_WRONG
    text.draw()
    win.flip()
    core.wait(FEEDBACK_DUR)
    

esc_key = 'escape' # quit everything

def quit():
    """
    Close everything and exit nicely (ending the experiment) and save a backup of relevant objects
    """
    objects_to_save = {
        "subjects": V['subject'],
        "trials": trials,
        "quest":quest,
        "duration_experiment": time.time() - start_experiment, # timer for the experiment
        "triggers_log": triglog
    }
    
    utils.save_objects(dirs["pkl"], V["subject"], objects_to_save)
    
    win.close()
    core.quit()

event.globalKeys.add(key=esc_key, func=quit)

"""
SET VARIABLES
"""

# Monitor parameters
MON_DISTANCE = 67  # Distance between subject's eyes and monitor
MON_WIDTH = 53.3  # Width of your monitor in cm
MON_SIZE = [1920, 1080]  # Pixel-dimensions of your monitor
REFRESH_RATE = 144 # monitor refresh rate (only for printing, no influence on performance)

# Stimulus parameters
GABOR_SF = 3.7  # 4 cycles per degree visual angle
GABOR_SIZE = 3.4  # in degrees visual angle
FIX_HEIGHT = 0.8  # Text height of fixation cross
H_OFFSET = 5 # horizontal offset from the center for gabor and masks in degree
V_OFFSET = 2 # vertical offset from the center for gabor and masks in degree

# Timings
FRAMES_FIX = 72  # in frames. ~ 500 ms on 144 hz
FRAMES_CUE = 72  # in frames. ~ 500 ms on 144 Hz
FRAMES_JITTER_LIST = [43, 50, 58, 65, 72] # in frames. ~ 300, 350, 400, 450, 500 jittering at 144hz
FRAMES_STIM = 5  # in frames. ~ 33 ms on 144 hz
FRAMES_MASK = 50  # in frames. ~ 350 ms on 144 hz
FRAMES_TARGET_RESP = 130 # in frames ~900 s on 144hz
ITI = 1.5 # in seconds, use core.wait TODO check for eeg
FEEDBACK_DUR = 1.5 # in seconds, use core.wait TODO check for eeg

DUR_DICT = {
    "fixation": FRAMES_FIX,
    "cue": FRAMES_CUE,
    "jitter1": FRAMES_JITTER_LIST[0],
    "jitter2": FRAMES_JITTER_LIST[1],
    "jitter3": FRAMES_JITTER_LIST[2],
    "jitter4": FRAMES_JITTER_LIST[3],
    "jitter5": FRAMES_JITTER_LIST[4],
    "gabor": FRAMES_STIM,
    "mask": FRAMES_MASK,
    "retention": FRAMES_TARGET_RESP
}

# Condition parameterss
POSITIONS = 0
ORIS = [15, 45, 75, 105, 135, 165]
MEMORY_TEST_DIFF = 50 # difference between memory and test in degrees
NTRIALS_BREAK = 40  # Number of trials between breaks
NTRIALS_PRAC = 10 # number of practice trials
NBLOCKS = 9 # number of blocks
CUE_SIDE = {"right":0, "left":180}

# Questions and messages
MESSAGE_POS = [0, 0]  # [x, y]
MESSAGE_HEIGHT = 1  # Height of the text, still in degrees visual angle

# Keys
KEYS_QUIT = ['escape']  # Keys that quits the experiment
PAS_RESP = {'1': 'pas1', '2': 'pas2', '3':'pas3', '4': 'pas4'} # pas keys
VIS_RESP = {'pas1': 0, 'pas2': 1, 'pas3': 1, 'pas4': 1} # keys for staircase from PAS RESP
TEST_RESP = {'f': '', 'j': ''} # keys for the change detection task
TEST_RESP_TXT = {'same': 'UGUALE', 'change': 'DIVERSO'}

"""
Instructions and Messages
"""

INSTR_WELCOME = """
    Benvenutə in questo esperimento!
    
    Premi la barra spaziatrice per continuare!
"""

INSTR_GENERAL = """
    In questo esperimento vedrai una croce di fissazione, seguita da una freccia che punta a sinistra o a destra. In seguito compariranno, a sinistra e a destra dello schermo, due stimoli visivi presentati molto velocemente. 
    
    Dovrai cercare di vedere e memorizzare lo stimolo indicato dala freccia. Dopo un breve intervallo (circa 1 secondo) comparirà un altro stimolo visivo nella parte di schermo indicata dalla freccia iniziale. Il tuo compito è quello di confrontare il primo stimolo con il secondo e poi riportare la tua esperienza visiva del primo stimolo (quello presentato velocemente)
    
    Premi la barra spaziatrice per ulteriori istruzioni
"""
    
INSTR_GABOR = """
    Gli stimoli che useremo si chiamano Gabor patch. Sono delle griglie circolari in bianco e nero, che possono essere ruotate con diversi orientamenti. Ecco un esempio:
"""

INSTR_MASKING = """
    La prima Gabor sarà presentata molto velocemente e seguita da un'altro stimolo formato da rumore visivo bianco e nero. Dovrai focalizzarti solo sulla Gabor e sul suo orientamento, il secondo stimolo non è rilevante.
    
    Essendo presentata velocemente, alcune volte sarà più difficile vederla e altre volte non vedrai proprio niente. Non ti preoccupare è del tutto normale.
"""

INSTR_MEMORY_PROBE = """
    Il tuo compito è quello di focalizzarti sulla prima Gabor e sul suo orientamento e cercare di memorizzarli. 
    
    Dopo un breve intervallo, dove dovrai mantenere sempre lo sguardo al centro, comparirà un'altra Gabor nello stesso lato dello schermo che potrà avere un orientamento UGUALE o DIVERSO rispetto a quello della prima Gabor. 
    
    Il tuo compito è rispondere il più accuratamente possibile decidendo se l'orientamento di questa seconda Gabor è uguale o diverso rispetto alla prima Gabor. 
    
    Se non hai visto la prima Gabor non ti preoccupare, tenta comunque di rispondere, eventualmente cercando di indovinare. 
    
    Premi F se l'orientamento è {f}
    Premi J se l'orientamento è {j}
"""

INSTR_PAS = """
    Infine ti sarà chiesto di riportare la tua ESPERIENZA VISIVA rispetto all'ORIENTAMENTO della prima Gabor (quella presentata velocemente). Non ci sono risposte giuste o sbagliate a questa domanda: siamo solo interessati a comprendere la tua esperienza il più accuratamente possibile
    
    Puoi usare queste opzioni di risposta:
    
    1 = Non ho visto l'orientamento
    2 = Ho la sensazione di aver visto l'orientamento
    3 = Ho visto abbastanza chiaramente l'orientamento
    4 = Ho visto chiaramente l'orientamento
"""

INSTR_EYE = """
    E' importante che tu mantenga lo sguardo al centro dello schermo senza muoverlo e senza sbattere le palpebre.
    
    Da quando compare la CROCE DI FISSAZIONE INIZIALE a quando RISPONDI UGUALE/DIVERSO dovresti:
        - mantenere lo sguardo al centro (no movimenti destra/sinistra o alto/  basso)
        - non sbattere gli occhi
    
    Dopo che avrai dato la risposta potrai muovere/sbattere gli occhi. Questo è molto importante ma comunque durante la pratica e l'esperimento ti daremo qualche feedback.
"""

INSTR_FEEDBACK = """
    Alcune volte ci saranno delle prove in cui la prima Gabor non verrà presentata. Dopo che avrai dato la risposta di visibilità solamente in queste prove ti sarà dato un feedback in funzione che tu dica di aver visto qualcosa oppure no. Questo serve soltanto affinchè tu possa adattare al meglio le tue risposte.
"""

FEEDBACK_POSITIVE = """
    Ottimo! non c'era la Gabor e hai risposto {}
"""

FEEDBACK_NEGATIVE = """
    Attenzione! non c'era la Gabor e hai risposto {}
"""

FEEDBACK_CORRECT = """
    Corretto!
"""

FEEDBACK_WRONG = """
    Sbagliato!
"""
    
END_PRAC_PAS = """
    La prima parte della pratica è finita!
    
    Se vuoi possiamo fare qualche altro trial di prova.
    
    Premi P per fare ancora un pochino di pratica
    Premi la barra spaziatrice per continuare con la seconda parte della pratica
"""

INSTR_START_EXPERIMENT = """
    La seconda parte della pratica è finita!
    
    Se vuoi possiamo fare qualche altro trial di prova.
    
    Premi P per fare ancora un pochino di pratica
    Premi la barra spaziatrice per cominciare l'esperimento
"""

PRAC_PAS_INSTRUCTIONS = """
    Prima di cominciare l'esperimento facciamo qualche trial di prova. 
    
    In questi trial sarà presentata solo la prima Gabor di cui dovrai riportare la tua esperienza visiva. Non verrà quindi presentata la seconda Gabor con il compito di memoria (uguale o diverso)
    
    Premi la barra spaziatrice per cominciare
"""

PRAC_INSTRUCTIONS = """
    Ottimo, ora facciamo qualche trial di prova inserendo anche la seconda Gabor che dovrai confrontare con la prima. Subito dopo ti verrà chiesto di riportare la tua esperienza visiva della prima Gabor. Ti ricordo i tasti di risposta:

    Premi F se l'orientamento è {f}
    Premi J se l'orientamento è {j}

    Premi la barra spaziatrice per cominciare
"""
    
PAS_RESPONSE = """
    1 = Non ho visto l'orientamento
    2 = Ho la sensazione di aver visto l'orientamento
    3 = Ho visto abbastanza chiaramente l'orientamento
    4 = Ho visto chiaramente l'orientamento
"""

END_EXPERIMENT = """
    L'esperimento è finito!
    
    Grazie per aver partecipato! :)
"""

TXT_BREAK = """
    Puoi prenderti una pausa!

    Premi la barra spaziatrice per continuare l'esperimento!
"""

"""
 SHOW DIALOGUE AND INITIATE PSYCHOPY STIMULI
 This is computationally heavy stuff. Thus we do it in the beginning of our experiment
"""

# Gabor expected size (check on the screen)

print("\n\nPARAMETERS CHECK \n\n")

print('the physical diameter of the gabor patch should be', utils.deg2cm(GABOR_SIZE, MON_DISTANCE), 'cm')
print('the physical size of the fixation cross should be', utils.deg2cm(FIX_HEIGHT, MON_DISTANCE), 'cm')

# debug the duration
for key, val in DUR_DICT.items():
    string = "The {} duration (ms) should be {}"
    print(string.format(key, str(utils.frames_to_dur(val, REFRESH_RATE))))

# Creating Project Structure

proj_structure = {
    "data_csv":os.path.join(os.path.curdir, "data", "csv"),
    "data_pkl":os.path.join(os.path.curdir, "data", "pkl"),
    "data_sim_csv":os.path.join(os.path.curdir, "data_sim", "csv"),
    "data_sim_pkl":os.path.join(os.path.curdir, "data_sim", "pkl")
}

utils.create_project_structure(proj_structure)

utils.step_completed("Project structure")

# Opening box for subject info

## triggers: True = use the parallel port, False = print the code and save on triglog

## practice: True = do the practice, False = skip the practice

## simulate: True = simulate an ideal observer, False = real participant

V = {'subject':'', 'age':'', 'gender':['male', 'female'], 'simulate':[False, True], 'triggers': [True, False], 'practice': [True, False]}

if not gui.DlgFromDict(V, order=['subject', 'age', 'gender']).OK:
    core.quit()

V['simulate'] = utils.str2bool(V['simulate']) # force to boolean from the gui
V['triggers'] = utils.str2bool(V['triggers']) # force to boolean from the gui
V['practice'] = utils.str2bool(V['practice']) # force to boolean from the gui

dirs = utils.get_dirs(simulate=V['simulate']) # get dirs according to simulation or real

# Setting CDT keys. Assign keys to odd and even subjects
if int(V['subject']) % 2 == 0: # if is even
    TEST_RESP['f'] = "same"
    TEST_RESP['j'] = "change"
else:
    TEST_RESP['f'] = "change"
    TEST_RESP['j'] = "same"
    
INSTR_MEMORY_PROBE = INSTR_MEMORY_PROBE.format(f = TEST_RESP_TXT[TEST_RESP['f']], j = TEST_RESP_TXT[TEST_RESP['j']])

PRAC_INSTRUCTIONS = PRAC_INSTRUCTIONS.format(f = TEST_RESP_TXT[TEST_RESP['f']], j = TEST_RESP_TXT[TEST_RESP['j']])

"""
Experiment Conditions
"""

cond = {
    "subject": [V['subject']],
    "age": [V['age']],
    "gender": [V["gender"]],
    "ntrial": [0],
    "memory_ori": ORIS,
    'test_ori': [0],
    'cue': ['left', 'right'],
    "type": ["change", "same"],
    "trial_type": ["valid", "catch"],
    "which_change": ["clock", "anti"],
    "pas": [''],
    "pas_rt": [0],
    "vis": [0],
    "test_key": [''],
    "test_acc": [''],
    "test": [''],
    "test_rt": [0],
    "contrast": [0],
    "fix_dur": [0],
    "cue_dur": [0],
    "jitter_dur": [0],
    "target_dur": [0],
    "mask_dur": [0],
    "retention_dur": [0],
    "trial_dur": [0]
}

trials, nvalid, ncatch = utils.create_conditions(cond, ncatch=10) # create the combinations using itertools. ncatch is the number of catch trials for each block

# assignin the test gabor orientation according to conditions. If catch a random test orientation is used

for trial in trials:
    if trial["trial_type"] == "valid":
        trial["test_ori"] = get_test_angle(target = trial["memory_ori"], diff = MEMORY_TEST_DIFF, type = trial["type"], which_change = trial["which_change"])
    else:
        trial["test_ori"] = get_test_angle(target = random.choice(ORIS), diff = MEMORY_TEST_DIFF, type = random.choice(["same", "change"]), which_change = random.choice(["clock", "anti"])) # random (realistic) test orientation for catch


utils.step_completed("Creating conditions")

"""
Creating the Window
"""

background_color = [-1,-1,-1] # ~ black

my_monitor = monitors.Monitor('testMonitor', width=MON_WIDTH, distance=MON_DISTANCE)  # Create monitor object from the variables above. This is needed to control size of stimuli in degrees.
my_monitor.setSizePix(MON_SIZE)

win = visual.Window(monitor=my_monitor, units='deg', fullscr=True, allowGUI=False, color=background_color)  # Initiate psychopy Window as the object "win", using the myMon object from last line. Use degree as units!
objects_color = "white"
win.mouseVisible = False # hide mouse

# Init the trial-by-trial saving function
writer = utils.csv_writer(cond, V["subject"], dirs["csv"])

"""
OBJECTS
"""

stim_pos = {
    "left":(-H_OFFSET, -V_OFFSET),
    "right":(H_OFFSET, -V_OFFSET)
}

fix = visual.TextStim(win, '+', height=FIX_HEIGHT)  # Fixation cross is just the character "+". Units are inherited from Window when not explicitly specified.

arrowVert = [(-0.15,0.01), (-0.15,-0.01), (0.10,-0.01), (0.10,-0.03), (0.15,0), (0.10,0.03), (0.10,0.01)] # vertices for the arrow https://github.com/psychopy/psychopy/blob/release/psychopy/demos/coder/stimuli/shapes.py

# convert to numpy to increase the size and use "deg" as unit
arrowVert = np.array(arrowVert)
arrowVert = arrowVert * 15

arrow_down = visual.ShapeStim(win, vertices = arrowVert, fillColor= objects_color, lineColor=objects_color, pos = (0, -V_OFFSET), units = "deg", size = 0.5)

gabor_prac = visual.GratingStim(win, mask='gauss', sf = GABOR_SF, size = GABOR_SIZE, pos = (0, 0))  # A gabor patch. Again, units are inherited.

gabor_memory_left = visual.GratingStim(win, mask='gauss', sf = GABOR_SF, size = GABOR_SIZE, pos = stim_pos['left'])  # A gabor patch. Again, units are inherited. # top-left

gabor_memory_right = visual.GratingStim(win, mask='gauss', sf = GABOR_SF, size = GABOR_SIZE, pos = stim_pos['right'])  # A gabor patch. Again, units are inherited. top-right

gabor_test = visual.GratingStim(win, mask='gauss', sf = GABOR_SF, size = GABOR_SIZE, pos = (0, 0), contrast = 1)  # A gabor patch. Again, units are inherited.

mask_left = visual.GratingStim(win, size=GABOR_SIZE, interpolate=False, autoLog=False, mask="circle", pos = stim_pos['left']) # mask for the backward masking. The texture is generated trial-by-trial

mask_right = visual.GratingStim(win, size=GABOR_SIZE, interpolate=False, autoLog=False, mask="circle", pos = stim_pos['right']) # mask for the backward masking. The texture is generated trial-by-trial

text = visual.TextStim(win, pos=MESSAGE_POS, height=MESSAGE_HEIGHT, wrapWidth=30)  # Message / question stimulus. Will be used to display instructions and questions.

#kb = keyboard.Keyboard() # init the keyboard
kb = None

# Paradigm Summary Image
paradigm_summary = visual.ImageStim(win, image="files/paradigm_summary.png", size = (600, 900), units="pix")

# Parallel Port setup

if V['triggers']:
    port = ParallelPort(address=0x0378) # the adress is 888
else:
    port = None

# Timing

clock_fix = utils.stimClock()
clock_cue = utils.stimClock()
clock_jitter = utils.stimClock()
clock_gabor = utils.stimClock()
clock_mask = utils.stimClock()
clock_retention = utils.stimClock()
clock_trial = core.Clock() # clock for the trial

# Triggers Log
triglog = utils.triggerLog() # object to save triggers for logging

# QUEST staircase

obs = utils.psy_observer(0.5, 0.2, 0, 0) # init ideal observer for simulation

fa_rate = 0
lapse_rate = 0.01

quest = data.QuestHandler(0.5, 0.2, beta = 3.5,
    pThreshold = 0.75, gamma = fa_rate, delta = lapse_rate,
    minVal=0, maxVal=1)

utils.step_completed("Creating objects")

################################################################################################################################################################

"""
Experiment
"""

def experiment(trials, ntrials = None, isPrac = False, isPracPAS = False, nblock = 1):
    """
    This function run the experiment or a subset of trials (for the practice). If the isPrac argument is True, a subset of random trials (ntrials) will be selected and used in the practice.
    """
    
    trials = random.sample(trials, len(trials)) # shuffling order of trials
    
    trial_counter = 0
    block_counter = nblock
    
    if isPrac or isPracPAS:
        prac_idx = random.sample(range(len(trials)), ntrials) # random index
        trials = [trials[i] for i in prac_idx] # select only practice trials

    # Start Experiment

    for i in range(len(trials)):
        
        trial_counter += 1 # increase trial counter
                
        ## -- TRIAL PREPARATION
        
        trial = trials[i] # get current dictionary
        
        # Check if catch and set contrast to 0, else take the QUEST
        
        if trial["trial_type"] == "catch":
            contrast_trial = 0
        else:
            if isPrac:
                contrast_trial = 1
            if isPracPAS:
                contrast_trial = random.uniform(0, 1)
            else:
                contrast_trial = quest._nextIntensity # suggest contrast
        
        obs.xi = contrast_trial # add contrast to observer
        
        # cue side
        arrow_down.ori = CUE_SIDE[trial["cue"]]
        
        # jitter dur
        FRAMES_JITTER = random.choice(FRAMES_JITTER_LIST)
        
        # memory orientation, position and contrast and trigger
        gabor_memory_left.contrast = contrast_trial
        gabor_memory_right.contrast = contrast_trial
        
        if trial["cue"] == "left":
            gabor_memory_left.ori = trial["memory_ori"]
            gabor_memory_right.ori = random.choice(ORIS)
        else:
            gabor_memory_right.ori = trial["memory_ori"]
            gabor_memory_left.ori = random.choice(ORIS)
            
        if trial["trial_type"] == "catch":
            memory_trigger  = triggers['catch']
        else:
            memory_trigger  = triggers["memory_ori"][ORIS.index(trial["memory_ori"])]
            
        # mask parameters and position
        tex_trial = np.random.rand(256, 256) * 2.0 - 1 # create numpy array
        mask_left.tex = tex_trial
        mask_right.tex = tex_trial
            
        # test orientation and position
        gabor_test.pos = stim_pos[trial["cue"]] # position of the test
        gabor_test.ori = trial['test_ori'] # assign ori to test
        
        # test trigger
        if trial["type"] == "same":
            test_trigger = triggers[trial["type"]]
        else:
            test_trigger = triggers[trial["which_change"]]
    
        ## -- ENDING TRIAL PREPARATION
        
        # -- BEGIN TRIAL
        
        clock_trial.reset()
        
        ## -- Fixation
        win.callOnFlip(sendTrigger, triggers["fixation"] + trial_counter, port = port, test = not V['triggers'])
        for frame in range(FRAMES_FIX):
            fix.draw()
            win.flip()
        
        ## -- Cue
        win.callOnFlip(sendTrigger, triggers[trial["cue"]], port = port, test = not V['triggers'])
        for frame in range(FRAMES_CUE):
            arrow_down.draw()
            fix.draw()
            win.flip()
        
        ## -- Jitter
        win.callOnFlip(sendTrigger, triggers["jitter"][FRAMES_JITTER_LIST.index(FRAMES_JITTER)], port = port, test = not V['triggers'])
        win.callOnFlip(clock_jitter.reset)
        for frame in range(FRAMES_JITTER):
            fix.draw()
            win.flip()
                
        ## -- Gabor
        win.callOnFlip(sendTrigger, memory_trigger, port = port, close = False, test = not V['triggers'])
        win.callOnFlip(clock_gabor.reset) # init gabor+mask clock
        
        for frame in range(FRAMES_STIM):
            gabor_memory_left.draw()
            gabor_memory_right.draw()
            win.flip()
        # here I open and close the port for not dropping frames
        closePort(port, test = not V['triggers'])
        
        ## -- Mask
        for frame in range(FRAMES_MASK):
            mask_left.draw()
            mask_right.draw()
            win.flip()
        
        ## -- Retention
        win.callOnFlip(sendTrigger, triggers["retention"] + block_counter, port = port, test = not V['triggers'])
        win.callOnFlip(clock_gabor.get_time) # get gabor+mask clock
        for frame in range(FRAMES_TARGET_RESP):
            fix.draw()
            win.flip()
            
        if not isPracPAS:
            
            ## -- Test
            win.callOnFlip(sendTrigger, test_trigger, port = port, test = not V['triggers'])
            gabor_test.draw() # drawing gabor for holding
            test_resp, test_rt = ask(kb, keyList = list(TEST_RESP.keys()), hold = True, simulate=V['simulate'])
            
            if test_resp != esc_key:
                if TEST_RESP[test_resp] == trial["type"]:
                    test_acc = 1
                    sendTrigger(triggers["test_correct"], port, test = not V['triggers'])
                else:
                    test_acc = 0
                    sendTrigger(triggers["test_wrong"], port, test = not V['triggers'])
        
        ## -- PAS
        
        pas_resp, pas_rt = ask(kb, text, PAS_RESPONSE, list(PAS_RESP.keys()), simulate=V['simulate'], obs=obs) # pas
        
        if pas_resp != esc_key:
            sendTrigger(triggers["pas"] + int(pas_resp), port, test = not V['triggers'])
        
        if trial["trial_type"] == "catch" or contrast_trial == 0:
            win.flip()
            feedback(pas_resp)
        
        ## -- ITI
        win.flip() # blank screen
        trial_dur = clock_trial.getTime() # end of trial
        core.wait(ITI) # intertrial interval
        
        # -- END TRIAL (stimuli)
        
        ## -- Update QUEST
        
        vis_resp = VIS_RESP[PAS_RESP[str(pas_resp)]] # coverting to 0-1
        
        # update and save only if valid and not prac or pracPAS
        
        if trial["trial_type"] == "valid" and not (isPrac or isPracPAS):
            quest.addResponse(vis_resp) # updating quest
            
        if not (isPrac or isPracPAS):
            ## -- Update Dict
            trial['contrast'] = contrast_trial
            trial['test_key'] = test_resp
            trial['test'] = TEST_RESP[test_resp]
            trial['test_acc'] = test_acc
            trial['test_rt'] = test_rt
            trial['pas'] = pas_resp
            trial['pas_rt'] = pas_rt
            trial['vis'] = vis_resp
            trial['ntrial'] = i + 1 # setting the correct trial number 
            
            ### -- timing
            trial['target_dur'] = clock_gabor.time # gabor + mask
            trial['trial_dur'] = trial_dur
        
            ## -- Saving Data
            writer.write(trial)
            
################################################################################################################################################################

"""
EXPERIMENT RUNNING
"""

# -- START EXPERIMENT

start_experiment = time.time() # timer for the overall experiment

## -- INSTRUCTIONS

# Welcome
ask(kb, text, INSTR_WELCOME, ['space'], simulate=V['simulate'])

# Instructions
ask(kb, text, INSTR_GENERAL, ['space'], simulate=V['simulate'])

# Gabor
gabor_memory_left.ori = 90
gabor_memory_right.ori = 10
gabor_memory_right.draw()
gabor_memory_left.draw()

ask(kb, text, INSTR_GABOR, ['space'], simulate=V['simulate'], pos = (0, 5))

# Masking
gabor_memory_left.draw()
mask_right.tex = np.random.rand(256, 256) * 2.0 - 1
mask_right.draw()

ask(kb, text, INSTR_MASKING, ['space'], simulate=V['simulate'], pos = (0, 5))

# Probe
ask(kb, text, INSTR_MEMORY_PROBE, ['space'], simulate=V['simulate'])

# PAS
ask(kb, text, INSTR_PAS, ['space'], simulate=V['simulate'])

# Feedback
ask(kb, text, INSTR_FEEDBACK, ['space'], simulate=V['simulate'])

# Paradigm Summary
paradigm_summary.draw()
ask(kb, text, "", ['space'], simulate=V['simulate'], hold = True)

if V["practice"]:

    ## -- PRACTICE PAS

    ask(kb, text, PRAC_PAS_INSTRUCTIONS, ['space'], simulate=V['simulate'])

    experiment(trials, ntrials = NTRIALS_PRAC, isPracPAS=True, nblock = 0)

    ### -- More practice?

    again_prac_pas = True

    while again_prac_pas:
        prac_key, prac_key_rt = ask(kb, text, END_PRAC_PAS, ['space', 'p'], simulate=V['simulate'])
        if prac_key == "p":
            experiment(trials, ntrials = NTRIALS_PRAC, isPracPAS=True, nblock = 0)
        else:
            again_prac_pas = False
            
    ## -- PRACTICE CDT

    ask(kb, text, PRAC_INSTRUCTIONS, ['space'], simulate=V['simulate'])

    experiment(trials, ntrials = NTRIALS_PRAC, isPrac=True, nblock = 0)
    
    ### -- More practice?

    again_prac = True

    while again_prac:
        prac_key, prac_key_rt = ask(kb, text, INSTR_START_EXPERIMENT, ['space', 'p'], simulate=V['simulate'])
        if prac_key == "p":
            experiment(trials, ntrials = NTRIALS_PRAC, isPrac=True, nblock = 0)
        else:
            again_prac = False
    
else:
    prac_key, prac_key_rt = ask(kb, text, INSTR_START_EXPERIMENT, ['space'], simulate=V['simulate'])

## -- EXPERIMENT

for block in range(NBLOCKS):
    experiment(trials, nblock = block + 1)
    # Break
    ask(kb, text, TXT_BREAK, ["space"], simulate=V['simulate'])

## -- ENDING

ask(kb, text, END_EXPERIMENT, ['space'], simulate=V['simulate'])
duration_experiment = time.time() - start_experiment # timer for the experiment

"""
SAVING DATA
"""

objects_to_save = {
    "subjects": V['subject'],
    "trials": trials,
    "quest": quest,
    "duration_experiment": duration_experiment,
    "triggers_log": triglog
}

utils.save_objects(dirs["pkl"], V["subject"], objects_to_save)

