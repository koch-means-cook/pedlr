#!/usr/bin/env python
# -*- coding: utf-8 -*-
"""
This experiment was created using PsychoPy3 Experiment Builder (v2020.1.2),
    on Mon Mar  9 12:12:21 2020
If you publish work using this script the most relevant publication is:

    Peirce J, Gray JR, Simpson S, MacAskill M, Höchenberger R, Sogo H, Kastman E, Lindeløv JK. (2019) 
        PsychoPy2: Experiments in behavior made easy Behav Res 51: 195. 
        https://doi.org/10.3758/s13428-018-01193-y

"""

from __future__ import absolute_import, division

from psychopy import locale_setup
from psychopy import prefs
prefs.hardware['audioLib'] = 'ptb'
from psychopy import sound, gui, visual, core, data, event, logging, clock
from psychopy.constants import (NOT_STARTED, STARTED, PLAYING, PAUSED,
                                STOPPED, FINISHED, PRESSED, RELEASED, FOREVER)

import numpy as np  # whole numpy lib is available, prepend 'np.'
from numpy import (sin, cos, tan, log, log10, pi, average,
                   sqrt, std, deg2rad, rad2deg, linspace, asarray)
from numpy.random import random, randint, normal, shuffle
import os  # handy system and path functions
import sys  # to get file system encoding

from psychopy.hardware import keyboard



# Ensure that relative paths start from the same directory as this script
_thisDir = os.path.dirname(os.path.abspath(__file__))
os.chdir(_thisDir)

# Store info about the experiment session
psychopyVersion = '2020.1.2'
expName = 'three_arm_bandit_rating_kb_block'  # from the Builder filename that created this script
expInfo = {'participant': '', 'age': '', 'gender': 'F'}
dlg = gui.DlgFromDict(dictionary=expInfo, sortKeys=False, title=expName)
if dlg.OK == False:
    core.quit()  # user pressed cancel
expInfo['date'] = data.getDateStr()  # add a simple timestamp
expInfo['expName'] = expName
expInfo['psychopyVersion'] = psychopyVersion

# Data file name stem = absolute path + name; later add .psyexp, .csv, .log, etc
filename = _thisDir + os.sep + 'data' + os.sep + '%s_%s' % (expInfo['participant'], expInfo['date'])

# An ExperimentHandler isn't essential but helps with data saving
thisExp = data.ExperimentHandler(name=expName, version='',
    extraInfo=expInfo, runtimeInfo=None,
    originPath='/Volumes/MPRG-Neurocode/Users/christoph/pedlr/code/task/three_bandit_rating_block_single_conterbalance_lastrun.py',
    savePickle=True, saveWideText=True,
    dataFileName=filename)
# save a log file for detail verbose info
logFile = logging.LogFile(filename+'.log', level=logging.WARNING)
logging.console.setLevel(logging.WARNING)  # this outputs to the screen, not a file

endExpNow = False  # flag for 'escape' or other condition => quit the exp
frameTolerance = 0.001  # how close to onset before 'same' frame

# Start Code - component code to be run before the window creation

# Setup the Window
win = visual.Window(
    size=[1024, 768], fullscr=False, screen=0, 
    winType='pyglet', allowGUI=True, allowStencil=False,
    monitor='testMonitor', color='black', colorSpace='rgb',
    blendMode='avg', useFBO=True, 
    units='height')
# store frame rate of monitor if we can measure it
expInfo['frameRate'] = win.getActualFrameRate()
if expInfo['frameRate'] != None:
    frameDur = 1.0 / round(expInfo['frameRate'])
else:
    frameDur = 1.0 / 60.0  # could not measure, so guess

# create a default keyboard (e.g. to check for escape)
defaultKeyboard = keyboard.Keyboard()

# Initialize components for Routine "pre_exp_instr"
pre_exp_instrClock = core.Clock()
msg_pre = visual.TextStim(win=win, name='msg_pre',
    text='Welcome!\n\nFirst of all\nYou will be shown some stimuli\nPlease rate how visually appealing\nthese stimuli are\n\nPress <left> or <right> to move the rating bar\nPress <return> to confirm\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_pre = keyboard.Keyboard()

# Initialize components for Routine "pre_exp_rating"
pre_exp_ratingClock = core.Clock()
pic_pre = visual.ImageStim(
    win=win,
    name='pic_pre', 
    image='sin', mask=None,
    ori=0, pos=(0, 0.2), size=(0.5, 0.5),
    color=[1,1,1], colorSpace='rgb', opacity=1.0,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating_bar_pre = visual.RatingScale(win=win, name='rating_bar_pre', scale=None,
low=0,
high=10,
precision=1,
markerStart=5,
labels = ('0','10'),
showValue=False, 
showAccept=False, 
acceptPreText='',
noMouse=True,
size=1.5,
minTime=0.1)

# Initialize components for Routine "p1_instr"
p1_instrClock = core.Clock()
msg_p1 = visual.TextStim(win=win, name='msg_p1',
    text='Free choice practice\n\nPress <right> and <left> arrow to choose a option.\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_p1 = keyboard.Keyboard()

# Initialize components for Routine "p1_free_c"
p1_free_cClock = core.Clock()
left_option_p1 = visual.ImageStim(
    win=win,
    name='left_option_p1', 
    image='sin', mask=None,
    ori=0, pos=(-0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
right_option_p1 = visual.ImageStim(
    win=win,
    name='right_option_p1', 
    image='sin', mask=None,
    ori=0, pos=(0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-1.0)
fixation_p1 = visual.ImageStim(
    win=win,
    name='fixation_p1', 
    image='stimuli/fixation.png', mask=None,
    ori=0, pos=(0, 0), size=(0.2, 0.2),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-2.0)
free_resp_p1 = keyboard.Keyboard()

# Initialize components for Routine "p1_fb"
p1_fbClock = core.Clock()
fb_p1 = visual.TextStim(win=win, name='fb_p1',
    text='default text',
    font='Arial',
    pos=[0, 0], height=0.1, wrapWidth=None, ori=0, 
    color=[1,1,1], colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "p1_end"
p1_endClock = core.Clock()
msg2_p1 = visual.TextStim(win=win, name='msg2_p1',
    text='If you understood free choice trials\nPress <right> to continue\n\nIf you would like to do the practice again\nPress <space>\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp2_p1 = keyboard.Keyboard()

# Initialize components for Routine "p2_instr"
p2_instrClock = core.Clock()
msg_p2 = visual.TextStim(win=win, name='msg_p2',
    text='Force choice practice\n\nIn this session, your task is to choose the option\ncovered by a grey square\n \nPress <right> and <left> arrow to choose a option\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_p2 = keyboard.Keyboard()

# Initialize components for Routine "p2_force_c"
p2_force_cClock = core.Clock()
left_option_p2 = visual.ImageStim(
    win=win,
    name='left_option_p2', 
    image='sin', mask=None,
    ori=0, pos=(-0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
right_option_p2 = visual.ImageStim(
    win=win,
    name='right_option_p2', 
    image='sin', mask=None,
    ori=0, pos=(0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-1.0)
fixation_p2 = visual.ImageStim(
    win=win,
    name='fixation_p2', 
    image='stimuli/fixation.png', mask=None,
    ori=0, pos=(0, 0), size=(0.2, 0.2),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-2.0)
force_resp_p2 = keyboard.Keyboard()
left_gray_p2 = visual.Rect(
    win=win, name='left_gray_p2',
    width=(0.25, 0.25)[0], height=(0.25, 0.25)[1],
    ori=0, pos=(-0.4, 0),
    lineWidth=1, lineColor=[0.7,0.7,0.7], lineColorSpace='rgb',
    fillColor=[0.7,0.7,0.7], fillColorSpace='rgb',
    opacity=0.7, depth=-4.0, interpolate=True)
right_gray_p2 = visual.Rect(
    win=win, name='right_gray_p2',
    width=(0.25, 0.25)[0], height=(0.25, 0.25)[1],
    ori=0, pos=(0.4, 0),
    lineWidth=1, lineColor=[0.7,0.7,0.7], lineColorSpace='rgb',
    fillColor=[0.7,0.7,0.7], fillColorSpace='rgb',
    opacity=0.7, depth=-5.0, interpolate=True)
fc_wrong_p2 = visual.TextStim(win=win, name='fc_wrong_p2',
    text='default text',
    font='Arial',
    pos=(0, 0.2), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-7.0);

# Initialize components for Routine "p2_fb"
p2_fbClock = core.Clock()
fb_p2 = visual.TextStim(win=win, name='fb_p2',
    text='default text',
    font='Arial',
    pos=[0, 0], height=0.1, wrapWidth=None, ori=0, 
    color=[1,1,1], colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "p2_end"
p2_endClock = core.Clock()
msg2_p2 = visual.TextStim(win=win, name='msg2_p2',
    text='If you understood free choice trials\nPress <right> to continue\n\nIf you would like to do the practice again\nPress <space>\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp2_p2 = keyboard.Keyboard()

# Initialize components for Routine "p3_instr"
p3_instrClock = core.Clock()
msg_p3 = visual.TextStim(win=win, name='msg_p3',
    text='Rating practice\n\nPlease rate the value of the following options\nbased on your experience in previous practice\n\nPress <right> and <left> arrow to move the rating bar\nPress <return> to confirm the rating\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_p3 = keyboard.Keyboard()

# Initialize components for Routine "p3_rating_2"
p3_rating_2Clock = core.Clock()
pic_p3 = visual.ImageStim(
    win=win,
    name='pic_p3', 
    image='sin', mask=None,
    ori=0, pos=(0, 0.2), size=(0.5, 0.5),
    color=[1,1,1], colorSpace='rgb', opacity=1.0,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating_bar_p3 = visual.RatingScale(win=win, name='rating_bar_p3', scale=None,
low=0,
high=10,
precision=2,
markerStart=5,
labels = ('0','50','100'),
showValue=False, 
showAccept=False, 
acceptPreText='',
noMouse=True,
size=1.5,
minTime=0.1)

# Initialize components for Routine "p3_end"
p3_endClock = core.Clock()
msg2_p3 = visual.TextStim(win=win, name='msg2_p3',
    text='If you understood free choice trials\nPress <right> to continue\n\nIf you would like to do the practice again\nPress <space>\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp2_p3 = keyboard.Keyboard()

# Initialize components for Routine "p4_instr"
p4_instrClock = core.Clock()
msg_p4 = visual.TextStim(win=win, name='msg_p4',
    text='A practice session mixing free choice, forced choice, and raings\n\nIt is similar to what you will do in the formal experiment\n\nPress <right> and <left> arrow to choose a option.\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_p4 = keyboard.Keyboard()

# Initialize components for Routine "p4_choice"
p4_choiceClock = core.Clock()
left_option_p4 = visual.ImageStim(
    win=win,
    name='left_option_p4', 
    image='sin', mask=None,
    ori=0, pos=(-0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
right_option_p4 = visual.ImageStim(
    win=win,
    name='right_option_p4', 
    image='sin', mask=None,
    ori=0, pos=(0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-1.0)
fixation_p4 = visual.ImageStim(
    win=win,
    name='fixation_p4', 
    image='stimuli/fixation.png', mask=None,
    ori=0, pos=(0, 0), size=(0.2, 0.2),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-2.0)
choice_p4 = keyboard.Keyboard()
left_gray_p4 = visual.Rect(
    win=win, name='left_gray_p4',
    width=(0.25, 0.25)[0], height=(0.25, 0.25)[1],
    ori=0, pos=(-0.4, 0),
    lineWidth=1, lineColor=[0.7,0.7,0.7], lineColorSpace='rgb',
    fillColor=[0.7,0.7,0.7], fillColorSpace='rgb',
    opacity=0.7, depth=-4.0, interpolate=True)
right_gray_p4 = visual.Rect(
    win=win, name='right_gray_p4',
    width=(0.25, 0.25)[0], height=(0.25, 0.25)[1],
    ori=0, pos=(0.4, 0),
    lineWidth=1, lineColor=[0.7,0.7,0.7], lineColorSpace='rgb',
    fillColor=[0.7,0.7,0.7], fillColorSpace='rgb',
    opacity=0.7, depth=-5.0, interpolate=True)
fc_wrong_p4 = visual.TextStim(win=win, name='fc_wrong_p4',
    text='default text',
    font='Arial',
    pos=(0, 0.2), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-7.0);

# Initialize components for Routine "p4_choice_fb"
p4_choice_fbClock = core.Clock()
fb_p4 = visual.TextStim(win=win, name='fb_p4',
    text='default text',
    font='Arial',
    pos=[0, 0], height=0.1, wrapWidth=None, ori=0, 
    color=[1,1,1], colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "p4_rating"
p4_ratingClock = core.Clock()
rating_p4 = visual.ImageStim(
    win=win,
    name='rating_p4', 
    image='sin', mask=None,
    ori=0, pos=(0, 0.2), size=(0.5, 0.5),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating_bar_p4 = visual.RatingScale(win=win, name='rating_bar_p4', scale=None,
low=0,
high=10,
precision=2,
markerStart=5,
labels = ('0','50','100'),
showValue=False, 
showAccept=False, 
acceptPreText='',
noMouse=True,
size=1.5,
minTime=0.1)
reminder_rating_p4 = visual.TextStim(win=win, name='reminder_rating_p4',
    text="Please press 'return' key to confirm your choice",
    font='Arial',
    pos=(0, -0.4), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);

# Initialize components for Routine "p4_end"
p4_endClock = core.Clock()
msg2_p4 = visual.TextStim(win=win, name='msg2_p4',
    text='If you understood free choice trials\nPress <right> to continue\n\nIf you would like to do the practice again\nPress <space>\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp2_p4 = keyboard.Keyboard()

# Initialize components for Routine "test_instr"
test_instrClock = core.Clock()
instr_resp = keyboard.Keyboard()
test_instr_txt = visual.TextStim(win=win, name='test_instr_txt',
    text='Test starts\n\nRules are the same as the last practice session.\n\nYou would be shown free choices, forced choices, or ratings,\nbut different stimuli will be used\n\nPress <right> to continue',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);

# Initialize components for Routine "break_bt_sess"
break_bt_sessClock = core.Clock()
break_text = visual.TextStim(win=win, name='break_text',
    text="Good work!\n\nNow you have 1 min a brief break.\n\nPlease press 'right' to continue if you are ready.",
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
break_resp = keyboard.Keyboard()

# Initialize components for Routine "new_instr"
new_instrClock = core.Clock()
new_instr_resp = keyboard.Keyboard()
new_instr_txt = visual.TextStim(win=win, name='new_instr_txt',
    text='You just finished a task!\n\nNext, you will see 3 new options\nThe ruls is the same as previous task\n\nPress <right> to start',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-2.0);

# Initialize components for Routine "test_choice"
test_choiceClock = core.Clock()
left_option = visual.ImageStim(
    win=win,
    name='left_option', 
    image='sin', mask=None,
    ori=0, pos=(-0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
right_option = visual.ImageStim(
    win=win,
    name='right_option', 
    image='sin', mask=None,
    ori=0, pos=(0.4, 0), size=(0.3, 0.3),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-1.0)
fixation = visual.ImageStim(
    win=win,
    name='fixation', 
    image='stimuli/fixation.png', mask=None,
    ori=0, pos=(0, 0), size=(0.2, 0.2),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=-2.0)
choice = keyboard.Keyboard()
left_gray = visual.Rect(
    win=win, name='left_gray',
    width=(0.25, 0.25)[0], height=(0.25, 0.25)[1],
    ori=0, pos=(-0.4, 0),
    lineWidth=1, lineColor=[0.7,0.7,0.7], lineColorSpace='rgb',
    fillColor=[0.7,0.7,0.7], fillColorSpace='rgb',
    opacity=0.7, depth=-4.0, interpolate=True)
right_gray = visual.Rect(
    win=win, name='right_gray',
    width=(0.25, 0.25)[0], height=(0.25, 0.25)[1],
    ori=0, pos=(0.4, 0),
    lineWidth=1, lineColor=[0.7,0.7,0.7], lineColorSpace='rgb',
    fillColor=[0.7,0.7,0.7], fillColorSpace='rgb',
    opacity=0.7, depth=-5.0, interpolate=True)
c_durat = 4
fc_wrong = visual.TextStim(win=win, name='fc_wrong',
    text='default text',
    font='Arial',
    pos=(0, 0.2), height=0.05, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-7.0);

# Initialize components for Routine "choice_fb"
choice_fbClock = core.Clock()
no_resp = float('nan')
wr_resp = 999
feedback_2 = visual.TextStim(win=win, name='feedback_2',
    text='default text',
    font='Arial',
    pos=[0, 0], height=0.1, wrapWidth=None, ori=0, 
    color=[1,1,1], colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-1.0);

# Initialize components for Routine "ITI"
ITIClock = core.Clock()
iti_fixation = visual.ImageStim(
    win=win,
    name='iti_fixation', 
    image='stimuli/fixation.png', mask=None,
    ori=0, pos=(0, 0), size=(0.2, 0.2),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)

# Initialize components for Routine "test_rating"
test_ratingClock = core.Clock()
option_rating1 = visual.ImageStim(
    win=win,
    name='option_rating1', 
    image='sin', mask=None,
    ori=0, pos=(0, 0.2), size=(0.5, 0.5),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating_bar1 = visual.RatingScale(win=win, name='rating_bar1', scale=None,
low=0,
high=10,
precision=2,
markerStart=5,
labels = ('0','50','100'),
showValue=False, 
showAccept=False, 
acceptPreText='',
noMouse=True,
size=1.5,
minTime=0.1)
reminder_rating1 = visual.TextStim(win=win, name='reminder_rating1',
    text="Please press 'return' key to confirm your choice",
    font='Arial',
    pos=(0, -0.4), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=-3.0);

# Initialize components for Routine "post_exp_instr1"
post_exp_instr1Clock = core.Clock()
msg_post1 = visual.TextStim(win=win, name='msg_post1',
    text='Good Job!\n\nNext, you need to rate again how visually \nappealing the following stimuli are\n\nPress <left> or <right> to move the rating bar\nPress <return> to confirm\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_post1 = keyboard.Keyboard()

# Initialize components for Routine "post_exp_rating1"
post_exp_rating1Clock = core.Clock()
pic_post1 = visual.ImageStim(
    win=win,
    name='pic_post1', 
    image='sin', mask=None,
    ori=0, pos=(0, 0.2), size=(0.5, 0.5),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating_bar_post1 = visual.RatingScale(win=win, name='rating_bar_post1', scale=None,
low=0,
high=10,
precision=1,
markerStart=5,
labels = ('0','10'),
showValue=False, 
showAccept=False, 
acceptPreText='',
noMouse=True,
size=1.5,
minTime=0.1)

# Initialize components for Routine "post_exp_instr2"
post_exp_instr2Clock = core.Clock()
msg_post2 = visual.TextStim(win=win, name='msg_post2',
    text='Next, you need to rate how much you like the\nfollowing stimuli given the experience \nyou had in the experiment\n\nPress <left> or <right> to move the rating bar\nPress <return> to confirm\n\nPress <right> to continue\n',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);
resp_post2 = keyboard.Keyboard()

# Initialize components for Routine "post_exp_rating2"
post_exp_rating2Clock = core.Clock()
pic_post2 = visual.ImageStim(
    win=win,
    name='pic_post2', 
    image='sin', mask=None,
    ori=0, pos=(0, 0.2), size=(0.5, 0.5),
    color=[1,1,1], colorSpace='rgb', opacity=1,
    flipHoriz=False, flipVert=False,
    texRes=128, interpolate=True, depth=0.0)
rating_bar_post2 = visual.RatingScale(win=win, name='rating_bar_post2', scale=None,
low=0,
high=10,
precision=1,
markerStart=5,
labels = ('0','10'),
showValue=False, 
showAccept=False, 
acceptPreText='',
noMouse=True,
size=1.5,
minTime=0.1)

# Initialize components for Routine "end"
endClock = core.Clock()
end_msg = visual.TextStim(win=win, name='end_msg',
    text='Thanks for your participation',
    font='Arial',
    pos=(0, 0), height=0.03, wrapWidth=None, ori=0, 
    color='white', colorSpace='rgb', opacity=1, 
    languageStyle='LTR',
    depth=0.0);

# Create some handy timers
globalClock = core.Clock()  # to track the time since experiment started
routineTimer = core.CountdownTimer()  # to track time remaining of each (non-slip) routine 

# ------Prepare to start Routine "pre_exp_instr"-------
continueRoutine = True
# update component parameters for each repeat
resp_pre.keys = []
resp_pre.rt = []
_resp_pre_allKeys = []
# keep track of which components have finished
pre_exp_instrComponents = [msg_pre, resp_pre]
for thisComponent in pre_exp_instrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
pre_exp_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "pre_exp_instr"-------
while continueRoutine:
    # get current time
    t = pre_exp_instrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=pre_exp_instrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_pre* updates
    if msg_pre.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_pre.frameNStart = frameN  # exact frame index
        msg_pre.tStart = t  # local t and not account for scr refresh
        msg_pre.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_pre, 'tStartRefresh')  # time at next scr refresh
        msg_pre.setAutoDraw(True)
    
    # *resp_pre* updates
    waitOnFlip = False
    if resp_pre.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_pre.frameNStart = frameN  # exact frame index
        resp_pre.tStart = t  # local t and not account for scr refresh
        resp_pre.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_pre, 'tStartRefresh')  # time at next scr refresh
        resp_pre.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_pre.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_pre.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_pre.status == STARTED and not waitOnFlip:
        theseKeys = resp_pre.getKeys(keyList=['right'], waitRelease=False)
        _resp_pre_allKeys.extend(theseKeys)
        if len(_resp_pre_allKeys):
            resp_pre.keys = _resp_pre_allKeys[-1].name  # just the last key pressed
            resp_pre.rt = _resp_pre_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in pre_exp_instrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "pre_exp_instr"-------
for thisComponent in pre_exp_instrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_pre.started', msg_pre.tStartRefresh)
thisExp.addData('msg_pre.stopped', msg_pre.tStopRefresh)
# the Routine "pre_exp_instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
pre_loop = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('pre_rating_file.xlsx'),
    seed=None, name='pre_loop')
thisExp.addLoop(pre_loop)  # add the loop to the experiment
thisPre_loop = pre_loop.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisPre_loop.rgb)
if thisPre_loop != None:
    for paramName in thisPre_loop:
        exec('{} = thisPre_loop[paramName]'.format(paramName))

for thisPre_loop in pre_loop:
    currentLoop = pre_loop
    # abbreviate parameter names if possible (e.g. rgb = thisPre_loop.rgb)
    if thisPre_loop != None:
        for paramName in thisPre_loop:
            exec('{} = thisPre_loop[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "pre_exp_rating"-------
    continueRoutine = True
    # update component parameters for each repeat
    pic_pre.setOpacity(1)
    pic_pre.setImage(pre_rating_pic)
    rating_bar_pre.reset()
    # keep track of which components have finished
    pre_exp_ratingComponents = [pic_pre, rating_bar_pre]
    for thisComponent in pre_exp_ratingComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    pre_exp_ratingClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "pre_exp_rating"-------
    while continueRoutine:
        # get current time
        t = pre_exp_ratingClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=pre_exp_ratingClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *pic_pre* updates
        if pic_pre.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            pic_pre.frameNStart = frameN  # exact frame index
            pic_pre.tStart = t  # local t and not account for scr refresh
            pic_pre.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(pic_pre, 'tStartRefresh')  # time at next scr refresh
            pic_pre.setAutoDraw(True)
        # *rating_bar_pre* updates
        if rating_bar_pre.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            rating_bar_pre.frameNStart = frameN  # exact frame index
            rating_bar_pre.tStart = t  # local t and not account for scr refresh
            rating_bar_pre.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(rating_bar_pre, 'tStartRefresh')  # time at next scr refresh
            rating_bar_pre.setAutoDraw(True)
        continueRoutine &= rating_bar_pre.noResponse  # a response ends the trial
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in pre_exp_ratingComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "pre_exp_rating"-------
    for thisComponent in pre_exp_ratingComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    pre_loop.addData('pic_pre.started', pic_pre.tStartRefresh)
    pre_loop.addData('pic_pre.stopped', pic_pre.tStopRefresh)
    # store data for pre_loop (TrialHandler)
    pre_loop.addData('rating_bar_pre.response', rating_bar_pre.getRating())
    pre_loop.addData('rating_bar_pre.rt', rating_bar_pre.getRT())
    pre_loop.addData('rating_bar_pre.started', rating_bar_pre.tStart)
    pre_loop.addData('rating_bar_pre.stopped', rating_bar_pre.tStop)
    # the Routine "pre_exp_rating" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'pre_loop'

# get names of stimulus parameters
if pre_loop.trialList in ([], [None], None):
    params = []
else:
    params = pre_loop.trialList[0].keys()
# save data for this loop
pre_loop.saveAsExcel(filename + '.xlsx', sheetName='pre_loop',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "p1_instr"-------
continueRoutine = True
# update component parameters for each repeat
resp_p1.keys = []
resp_p1.rt = []
_resp_p1_allKeys = []
# keep track of which components have finished
p1_instrComponents = [msg_p1, resp_p1]
for thisComponent in p1_instrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
p1_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "p1_instr"-------
while continueRoutine:
    # get current time
    t = p1_instrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=p1_instrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_p1* updates
    if msg_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_p1.frameNStart = frameN  # exact frame index
        msg_p1.tStart = t  # local t and not account for scr refresh
        msg_p1.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_p1, 'tStartRefresh')  # time at next scr refresh
        msg_p1.setAutoDraw(True)
    
    # *resp_p1* updates
    waitOnFlip = False
    if resp_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_p1.frameNStart = frameN  # exact frame index
        resp_p1.tStart = t  # local t and not account for scr refresh
        resp_p1.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_p1, 'tStartRefresh')  # time at next scr refresh
        resp_p1.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_p1.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_p1.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_p1.status == STARTED and not waitOnFlip:
        theseKeys = resp_p1.getKeys(keyList=['right'], waitRelease=False)
        _resp_p1_allKeys.extend(theseKeys)
        if len(_resp_p1_allKeys):
            resp_p1.keys = _resp_p1_allKeys[-1].name  # just the last key pressed
            resp_p1.rt = _resp_p1_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in p1_instrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "p1_instr"-------
for thisComponent in p1_instrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_p1.started', msg_p1.tStartRefresh)
thisExp.addData('msg_p1.stopped', msg_p1.tStopRefresh)
# the Routine "p1_instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
p1_repeat = data.TrialHandler(nReps=5, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('p1_repeat.xlsx'),
    seed=None, name='p1_repeat')
thisExp.addLoop(p1_repeat)  # add the loop to the experiment
thisP1_repeat = p1_repeat.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisP1_repeat.rgb)
if thisP1_repeat != None:
    for paramName in thisP1_repeat:
        exec('{} = thisP1_repeat[paramName]'.format(paramName))

for thisP1_repeat in p1_repeat:
    currentLoop = p1_repeat
    # abbreviate parameter names if possible (e.g. rgb = thisP1_repeat.rgb)
    if thisP1_repeat != None:
        for paramName in thisP1_repeat:
            exec('{} = thisP1_repeat[paramName]'.format(paramName))
    
    # set up handler to look after randomisation of conditions etc
    p1_loop = data.TrialHandler(nReps=1, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions(p1_file_name),
        seed=None, name='p1_loop')
    thisExp.addLoop(p1_loop)  # add the loop to the experiment
    thisP1_loop = p1_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisP1_loop.rgb)
    if thisP1_loop != None:
        for paramName in thisP1_loop:
            exec('{} = thisP1_loop[paramName]'.format(paramName))
    
    for thisP1_loop in p1_loop:
        currentLoop = p1_loop
        # abbreviate parameter names if possible (e.g. rgb = thisP1_loop.rgb)
        if thisP1_loop != None:
            for paramName in thisP1_loop:
                exec('{} = thisP1_loop[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "p1_free_c"-------
        continueRoutine = True
        # update component parameters for each repeat
        left_option_p1.setImage(p1_pic_left)
        right_option_p1.setImage(p1_pic_right)
        free_resp_p1.keys = []
        free_resp_p1.rt = []
        _free_resp_p1_allKeys = []
        # keep track of which components have finished
        p1_free_cComponents = [left_option_p1, right_option_p1, fixation_p1, free_resp_p1]
        for thisComponent in p1_free_cComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p1_free_cClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p1_free_c"-------
        while continueRoutine:
            # get current time
            t = p1_free_cClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p1_free_cClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *left_option_p1* updates
            if left_option_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                left_option_p1.frameNStart = frameN  # exact frame index
                left_option_p1.tStart = t  # local t and not account for scr refresh
                left_option_p1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(left_option_p1, 'tStartRefresh')  # time at next scr refresh
                left_option_p1.setAutoDraw(True)
            
            # *right_option_p1* updates
            if right_option_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                right_option_p1.frameNStart = frameN  # exact frame index
                right_option_p1.tStart = t  # local t and not account for scr refresh
                right_option_p1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(right_option_p1, 'tStartRefresh')  # time at next scr refresh
                right_option_p1.setAutoDraw(True)
            
            # *fixation_p1* updates
            if fixation_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation_p1.frameNStart = frameN  # exact frame index
                fixation_p1.tStart = t  # local t and not account for scr refresh
                fixation_p1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation_p1, 'tStartRefresh')  # time at next scr refresh
                fixation_p1.setAutoDraw(True)
            
            # *free_resp_p1* updates
            waitOnFlip = False
            if free_resp_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                free_resp_p1.frameNStart = frameN  # exact frame index
                free_resp_p1.tStart = t  # local t and not account for scr refresh
                free_resp_p1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(free_resp_p1, 'tStartRefresh')  # time at next scr refresh
                free_resp_p1.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(free_resp_p1.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(free_resp_p1.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if free_resp_p1.status == STARTED and not waitOnFlip:
                theseKeys = free_resp_p1.getKeys(keyList=['left', 'right'], waitRelease=False)
                _free_resp_p1_allKeys.extend(theseKeys)
                if len(_free_resp_p1_allKeys):
                    free_resp_p1.keys = _free_resp_p1_allKeys[-1].name  # just the last key pressed
                    free_resp_p1.rt = _free_resp_p1_allKeys[-1].rt
                    # a response ends the routine
                    continueRoutine = False
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p1_free_cComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p1_free_c"-------
        for thisComponent in p1_free_cComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p1_loop.addData('left_option_p1.started', left_option_p1.tStartRefresh)
        p1_loop.addData('left_option_p1.stopped', left_option_p1.tStopRefresh)
        p1_loop.addData('right_option_p1.started', right_option_p1.tStartRefresh)
        p1_loop.addData('right_option_p1.stopped', right_option_p1.tStopRefresh)
        p1_loop.addData('fixation_p1.started', fixation_p1.tStartRefresh)
        p1_loop.addData('fixation_p1.stopped', fixation_p1.tStopRefresh)
        # the Routine "p1_free_c" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # ------Prepare to start Routine "p1_fb"-------
        continueRoutine = True
        routineTimer.add(1.000000)
        # update component parameters for each repeat
        if free_resp_p1.keys=="left":#stored on last run routine
          msg_p1=p1_reward_1
        elif free_resp_p1.keys=="right":
          msg_p1=p1_reward_2
        fb_p1.setText(msg_p1)
        # keep track of which components have finished
        p1_fbComponents = [fb_p1]
        for thisComponent in p1_fbComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p1_fbClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p1_fb"-------
        while continueRoutine and routineTimer.getTime() > 0:
            # get current time
            t = p1_fbClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p1_fbClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *fb_p1* updates
            if fb_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fb_p1.frameNStart = frameN  # exact frame index
                fb_p1.tStart = t  # local t and not account for scr refresh
                fb_p1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fb_p1, 'tStartRefresh')  # time at next scr refresh
                fb_p1.setAutoDraw(True)
            if fb_p1.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > fb_p1.tStartRefresh + 1-frameTolerance:
                    # keep track of stop time/frame for later
                    fb_p1.tStop = t  # not accounting for scr refresh
                    fb_p1.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(fb_p1, 'tStopRefresh')  # time at next scr refresh
                    fb_p1.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p1_fbComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p1_fb"-------
        for thisComponent in p1_fbComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p1_loop.addData('fb_p1.started', fb_p1.tStartRefresh)
        p1_loop.addData('fb_p1.stopped', fb_p1.tStopRefresh)
        thisExp.nextEntry()
        
    # completed 1 repeats of 'p1_loop'
    
    # get names of stimulus parameters
    if p1_loop.trialList in ([], [None], None):
        params = []
    else:
        params = p1_loop.trialList[0].keys()
    # save data for this loop
    p1_loop.saveAsExcel(filename + '.xlsx', sheetName='p1_loop',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    
    # ------Prepare to start Routine "p1_end"-------
    continueRoutine = True
    # update component parameters for each repeat
    resp2_p1.keys = []
    resp2_p1.rt = []
    _resp2_p1_allKeys = []
    # keep track of which components have finished
    p1_endComponents = [msg2_p1, resp2_p1]
    for thisComponent in p1_endComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    p1_endClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "p1_end"-------
    while continueRoutine:
        # get current time
        t = p1_endClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=p1_endClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *msg2_p1* updates
        if msg2_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            msg2_p1.frameNStart = frameN  # exact frame index
            msg2_p1.tStart = t  # local t and not account for scr refresh
            msg2_p1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(msg2_p1, 'tStartRefresh')  # time at next scr refresh
            msg2_p1.setAutoDraw(True)
        
        # *resp2_p1* updates
        waitOnFlip = False
        if resp2_p1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp2_p1.frameNStart = frameN  # exact frame index
            resp2_p1.tStart = t  # local t and not account for scr refresh
            resp2_p1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp2_p1, 'tStartRefresh')  # time at next scr refresh
            resp2_p1.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp2_p1.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp2_p1.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp2_p1.status == STARTED and not waitOnFlip:
            theseKeys = resp2_p1.getKeys(keyList=['space', 'right'], waitRelease=False)
            _resp2_p1_allKeys.extend(theseKeys)
            if len(_resp2_p1_allKeys):
                resp2_p1.keys = _resp2_p1_allKeys[-1].name  # just the last key pressed
                resp2_p1.rt = _resp2_p1_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        if resp2_p1.keys == "right":
                p1_repeat.finished = True
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in p1_endComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "p1_end"-------
    for thisComponent in p1_endComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    p1_repeat.addData('msg2_p1.started', msg2_p1.tStartRefresh)
    p1_repeat.addData('msg2_p1.stopped', msg2_p1.tStopRefresh)
    # the Routine "p1_end" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 5 repeats of 'p1_repeat'

# get names of stimulus parameters
if p1_repeat.trialList in ([], [None], None):
    params = []
else:
    params = p1_repeat.trialList[0].keys()
# save data for this loop
p1_repeat.saveAsExcel(filename + '.xlsx', sheetName='p1_repeat',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "p2_instr"-------
continueRoutine = True
# update component parameters for each repeat
resp_p2.keys = []
resp_p2.rt = []
_resp_p2_allKeys = []
# keep track of which components have finished
p2_instrComponents = [msg_p2, resp_p2]
for thisComponent in p2_instrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
p2_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "p2_instr"-------
while continueRoutine:
    # get current time
    t = p2_instrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=p2_instrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_p2* updates
    if msg_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_p2.frameNStart = frameN  # exact frame index
        msg_p2.tStart = t  # local t and not account for scr refresh
        msg_p2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_p2, 'tStartRefresh')  # time at next scr refresh
        msg_p2.setAutoDraw(True)
    
    # *resp_p2* updates
    waitOnFlip = False
    if resp_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_p2.frameNStart = frameN  # exact frame index
        resp_p2.tStart = t  # local t and not account for scr refresh
        resp_p2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_p2, 'tStartRefresh')  # time at next scr refresh
        resp_p2.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_p2.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_p2.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_p2.status == STARTED and not waitOnFlip:
        theseKeys = resp_p2.getKeys(keyList=['right'], waitRelease=False)
        _resp_p2_allKeys.extend(theseKeys)
        if len(_resp_p2_allKeys):
            resp_p2.keys = _resp_p2_allKeys[-1].name  # just the last key pressed
            resp_p2.rt = _resp_p2_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in p2_instrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "p2_instr"-------
for thisComponent in p2_instrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_p2.started', msg_p2.tStartRefresh)
thisExp.addData('msg_p2.stopped', msg_p2.tStopRefresh)
# the Routine "p2_instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
p2_repeat = data.TrialHandler(nReps=20, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('p2_repeat.xlsx'),
    seed=None, name='p2_repeat')
thisExp.addLoop(p2_repeat)  # add the loop to the experiment
thisP2_repeat = p2_repeat.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisP2_repeat.rgb)
if thisP2_repeat != None:
    for paramName in thisP2_repeat:
        exec('{} = thisP2_repeat[paramName]'.format(paramName))

for thisP2_repeat in p2_repeat:
    currentLoop = p2_repeat
    # abbreviate parameter names if possible (e.g. rgb = thisP2_repeat.rgb)
    if thisP2_repeat != None:
        for paramName in thisP2_repeat:
            exec('{} = thisP2_repeat[paramName]'.format(paramName))
    
    # set up handler to look after randomisation of conditions etc
    p2_loop = data.TrialHandler(nReps=1, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions(p2_file_name),
        seed=None, name='p2_loop')
    thisExp.addLoop(p2_loop)  # add the loop to the experiment
    thisP2_loop = p2_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisP2_loop.rgb)
    if thisP2_loop != None:
        for paramName in thisP2_loop:
            exec('{} = thisP2_loop[paramName]'.format(paramName))
    
    for thisP2_loop in p2_loop:
        currentLoop = p2_loop
        # abbreviate parameter names if possible (e.g. rgb = thisP2_loop.rgb)
        if thisP2_loop != None:
            for paramName in thisP2_loop:
                exec('{} = thisP2_loop[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "p2_force_c"-------
        continueRoutine = True
        # update component parameters for each repeat
        left_option_p2.setImage(p2_pic_left)
        right_option_p2.setImage(p2_pic_right)
        force_resp_p2.keys = []
        force_resp_p2.rt = []
        _force_resp_p2_allKeys = []
        p2_wrong_ms=''
        # keep track of which components have finished
        p2_force_cComponents = [left_option_p2, right_option_p2, fixation_p2, force_resp_p2, left_gray_p2, right_gray_p2, fc_wrong_p2]
        for thisComponent in p2_force_cComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p2_force_cClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p2_force_c"-------
        while continueRoutine:
            # get current time
            t = p2_force_cClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p2_force_cClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *left_option_p2* updates
            if left_option_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                left_option_p2.frameNStart = frameN  # exact frame index
                left_option_p2.tStart = t  # local t and not account for scr refresh
                left_option_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(left_option_p2, 'tStartRefresh')  # time at next scr refresh
                left_option_p2.setAutoDraw(True)
            
            # *right_option_p2* updates
            if right_option_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                right_option_p2.frameNStart = frameN  # exact frame index
                right_option_p2.tStart = t  # local t and not account for scr refresh
                right_option_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(right_option_p2, 'tStartRefresh')  # time at next scr refresh
                right_option_p2.setAutoDraw(True)
            
            # *fixation_p2* updates
            if fixation_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation_p2.frameNStart = frameN  # exact frame index
                fixation_p2.tStart = t  # local t and not account for scr refresh
                fixation_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation_p2, 'tStartRefresh')  # time at next scr refresh
                fixation_p2.setAutoDraw(True)
            
            # *force_resp_p2* updates
            waitOnFlip = False
            if force_resp_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                force_resp_p2.frameNStart = frameN  # exact frame index
                force_resp_p2.tStart = t  # local t and not account for scr refresh
                force_resp_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(force_resp_p2, 'tStartRefresh')  # time at next scr refresh
                force_resp_p2.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(force_resp_p2.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(force_resp_p2.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if force_resp_p2.status == STARTED and not waitOnFlip:
                theseKeys = force_resp_p2.getKeys(keyList=['left', 'right'], waitRelease=False)
                _force_resp_p2_allKeys.extend(theseKeys)
                if len(_force_resp_p2_allKeys):
                    force_resp_p2.keys = _force_resp_p2_allKeys[-1].name  # just the last key pressed
                    force_resp_p2.rt = _force_resp_p2_allKeys[-1].rt
            
            # *left_gray_p2* updates
            if left_gray_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                left_gray_p2.frameNStart = frameN  # exact frame index
                left_gray_p2.tStart = t  # local t and not account for scr refresh
                left_gray_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(left_gray_p2, 'tStartRefresh')  # time at next scr refresh
                left_gray_p2.setAutoDraw(True)
            if left_gray_p2.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > left_gray_p2.tStartRefresh + p2_forced_left*100-frameTolerance:
                    # keep track of stop time/frame for later
                    left_gray_p2.tStop = t  # not accounting for scr refresh
                    left_gray_p2.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(left_gray_p2, 'tStopRefresh')  # time at next scr refresh
                    left_gray_p2.setAutoDraw(False)
            
            # *right_gray_p2* updates
            if right_gray_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                right_gray_p2.frameNStart = frameN  # exact frame index
                right_gray_p2.tStart = t  # local t and not account for scr refresh
                right_gray_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(right_gray_p2, 'tStartRefresh')  # time at next scr refresh
                right_gray_p2.setAutoDraw(True)
            if right_gray_p2.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > right_gray_p2.tStartRefresh + p2_forced_right*100-frameTolerance:
                    # keep track of stop time/frame for later
                    right_gray_p2.tStop = t  # not accounting for scr refresh
                    right_gray_p2.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(right_gray_p2, 'tStopRefresh')  # time at next scr refresh
                    right_gray_p2.setAutoDraw(False)
            if (p2_forced_left and force_resp_p2.keys=="right") or (p2_forced_right and force_resp_p2.keys=="left"):
                p2_wrong_ms='Please select the cued option'
            elif (force_resp_p2.keys=="left") or (force_resp_p2.keys=="right"):
                continueRoutine = False
            
            # *fc_wrong_p2* updates
            if fc_wrong_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fc_wrong_p2.frameNStart = frameN  # exact frame index
                fc_wrong_p2.tStart = t  # local t and not account for scr refresh
                fc_wrong_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fc_wrong_p2, 'tStartRefresh')  # time at next scr refresh
                fc_wrong_p2.setAutoDraw(True)
            if fc_wrong_p2.status == STARTED:  # only update if drawing
                fc_wrong_p2.setText(p2_wrong_ms, log=False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p2_force_cComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p2_force_c"-------
        for thisComponent in p2_force_cComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p2_loop.addData('left_option_p2.started', left_option_p2.tStartRefresh)
        p2_loop.addData('left_option_p2.stopped', left_option_p2.tStopRefresh)
        p2_loop.addData('right_option_p2.started', right_option_p2.tStartRefresh)
        p2_loop.addData('right_option_p2.stopped', right_option_p2.tStopRefresh)
        p2_loop.addData('fixation_p2.started', fixation_p2.tStartRefresh)
        p2_loop.addData('fixation_p2.stopped', fixation_p2.tStopRefresh)
        p2_loop.addData('left_gray_p2.started', left_gray_p2.tStartRefresh)
        p2_loop.addData('left_gray_p2.stopped', left_gray_p2.tStopRefresh)
        p2_loop.addData('right_gray_p2.started', right_gray_p2.tStartRefresh)
        p2_loop.addData('right_gray_p2.stopped', right_gray_p2.tStopRefresh)
        p2_loop.addData('fc_wrong_p2.started', fc_wrong_p2.tStartRefresh)
        p2_loop.addData('fc_wrong_p2.stopped', fc_wrong_p2.tStopRefresh)
        # the Routine "p2_force_c" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # ------Prepare to start Routine "p2_fb"-------
        continueRoutine = True
        routineTimer.add(1.000000)
        # update component parameters for each repeat
        if force_resp_p2.keys=="left":#stored on last run routine
          msg_p2 = p2_reward_1
        elif force_resp_p2.keys=="right":
          msg_p2 = p2_reward_2
        else:# if no key was pressed
          msg_p2 ='No response'
        fb_p2.setText(msg_p2)
        # keep track of which components have finished
        p2_fbComponents = [fb_p2]
        for thisComponent in p2_fbComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p2_fbClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p2_fb"-------
        while continueRoutine and routineTimer.getTime() > 0:
            # get current time
            t = p2_fbClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p2_fbClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *fb_p2* updates
            if fb_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fb_p2.frameNStart = frameN  # exact frame index
                fb_p2.tStart = t  # local t and not account for scr refresh
                fb_p2.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fb_p2, 'tStartRefresh')  # time at next scr refresh
                fb_p2.setAutoDraw(True)
            if fb_p2.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > fb_p2.tStartRefresh + 1-frameTolerance:
                    # keep track of stop time/frame for later
                    fb_p2.tStop = t  # not accounting for scr refresh
                    fb_p2.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(fb_p2, 'tStopRefresh')  # time at next scr refresh
                    fb_p2.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p2_fbComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p2_fb"-------
        for thisComponent in p2_fbComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p2_loop.addData('fb_p2.started', fb_p2.tStartRefresh)
        p2_loop.addData('fb_p2.stopped', fb_p2.tStopRefresh)
        thisExp.nextEntry()
        
    # completed 1 repeats of 'p2_loop'
    
    # get names of stimulus parameters
    if p2_loop.trialList in ([], [None], None):
        params = []
    else:
        params = p2_loop.trialList[0].keys()
    # save data for this loop
    p2_loop.saveAsExcel(filename + '.xlsx', sheetName='p2_loop',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    
    # ------Prepare to start Routine "p2_end"-------
    continueRoutine = True
    # update component parameters for each repeat
    resp2_p2.keys = []
    resp2_p2.rt = []
    _resp2_p2_allKeys = []
    # keep track of which components have finished
    p2_endComponents = [msg2_p2, resp2_p2]
    for thisComponent in p2_endComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    p2_endClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "p2_end"-------
    while continueRoutine:
        # get current time
        t = p2_endClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=p2_endClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *msg2_p2* updates
        if msg2_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            msg2_p2.frameNStart = frameN  # exact frame index
            msg2_p2.tStart = t  # local t and not account for scr refresh
            msg2_p2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(msg2_p2, 'tStartRefresh')  # time at next scr refresh
            msg2_p2.setAutoDraw(True)
        
        # *resp2_p2* updates
        waitOnFlip = False
        if resp2_p2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp2_p2.frameNStart = frameN  # exact frame index
            resp2_p2.tStart = t  # local t and not account for scr refresh
            resp2_p2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp2_p2, 'tStartRefresh')  # time at next scr refresh
            resp2_p2.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp2_p2.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp2_p2.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp2_p2.status == STARTED and not waitOnFlip:
            theseKeys = resp2_p2.getKeys(keyList=['space', 'right'], waitRelease=False)
            _resp2_p2_allKeys.extend(theseKeys)
            if len(_resp2_p2_allKeys):
                resp2_p2.keys = _resp2_p2_allKeys[-1].name  # just the last key pressed
                resp2_p2.rt = _resp2_p2_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        if resp2_p2.keys == "right":
                p2_repeat.finished = True
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in p2_endComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "p2_end"-------
    for thisComponent in p2_endComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    p2_repeat.addData('msg2_p2.started', msg2_p2.tStartRefresh)
    p2_repeat.addData('msg2_p2.stopped', msg2_p2.tStopRefresh)
    # the Routine "p2_end" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 20 repeats of 'p2_repeat'

# get names of stimulus parameters
if p2_repeat.trialList in ([], [None], None):
    params = []
else:
    params = p2_repeat.trialList[0].keys()
# save data for this loop
p2_repeat.saveAsExcel(filename + '.xlsx', sheetName='p2_repeat',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "p3_instr"-------
continueRoutine = True
# update component parameters for each repeat
resp_p3.keys = []
resp_p3.rt = []
_resp_p3_allKeys = []
# keep track of which components have finished
p3_instrComponents = [msg_p3, resp_p3]
for thisComponent in p3_instrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
p3_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "p3_instr"-------
while continueRoutine:
    # get current time
    t = p3_instrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=p3_instrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_p3* updates
    if msg_p3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_p3.frameNStart = frameN  # exact frame index
        msg_p3.tStart = t  # local t and not account for scr refresh
        msg_p3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_p3, 'tStartRefresh')  # time at next scr refresh
        msg_p3.setAutoDraw(True)
    
    # *resp_p3* updates
    waitOnFlip = False
    if resp_p3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_p3.frameNStart = frameN  # exact frame index
        resp_p3.tStart = t  # local t and not account for scr refresh
        resp_p3.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_p3, 'tStartRefresh')  # time at next scr refresh
        resp_p3.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_p3.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_p3.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_p3.status == STARTED and not waitOnFlip:
        theseKeys = resp_p3.getKeys(keyList=['right'], waitRelease=False)
        _resp_p3_allKeys.extend(theseKeys)
        if len(_resp_p3_allKeys):
            resp_p3.keys = _resp_p3_allKeys[-1].name  # just the last key pressed
            resp_p3.rt = _resp_p3_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in p3_instrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "p3_instr"-------
for thisComponent in p3_instrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_p3.started', msg_p3.tStartRefresh)
thisExp.addData('msg_p3.stopped', msg_p3.tStopRefresh)
# the Routine "p3_instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
p3_repeat = data.TrialHandler(nReps=100, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('p3_repeat.xlsx'),
    seed=None, name='p3_repeat')
thisExp.addLoop(p3_repeat)  # add the loop to the experiment
thisP3_repeat = p3_repeat.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisP3_repeat.rgb)
if thisP3_repeat != None:
    for paramName in thisP3_repeat:
        exec('{} = thisP3_repeat[paramName]'.format(paramName))

for thisP3_repeat in p3_repeat:
    currentLoop = p3_repeat
    # abbreviate parameter names if possible (e.g. rgb = thisP3_repeat.rgb)
    if thisP3_repeat != None:
        for paramName in thisP3_repeat:
            exec('{} = thisP3_repeat[paramName]'.format(paramName))
    
    # set up handler to look after randomisation of conditions etc
    p3_loop = data.TrialHandler(nReps=1, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions(p3_file_name),
        seed=None, name='p3_loop')
    thisExp.addLoop(p3_loop)  # add the loop to the experiment
    thisP3_loop = p3_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisP3_loop.rgb)
    if thisP3_loop != None:
        for paramName in thisP3_loop:
            exec('{} = thisP3_loop[paramName]'.format(paramName))
    
    for thisP3_loop in p3_loop:
        currentLoop = p3_loop
        # abbreviate parameter names if possible (e.g. rgb = thisP3_loop.rgb)
        if thisP3_loop != None:
            for paramName in thisP3_loop:
                exec('{} = thisP3_loop[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "p3_rating_2"-------
        continueRoutine = True
        # update component parameters for each repeat
        pic_p3.setOpacity(1)
        pic_p3.setImage(p3_pic_rating)
        rating_bar_p3.reset()
        # keep track of which components have finished
        p3_rating_2Components = [pic_p3, rating_bar_p3]
        for thisComponent in p3_rating_2Components:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p3_rating_2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p3_rating_2"-------
        while continueRoutine:
            # get current time
            t = p3_rating_2Clock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p3_rating_2Clock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *pic_p3* updates
            if pic_p3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                pic_p3.frameNStart = frameN  # exact frame index
                pic_p3.tStart = t  # local t and not account for scr refresh
                pic_p3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(pic_p3, 'tStartRefresh')  # time at next scr refresh
                pic_p3.setAutoDraw(True)
            # *rating_bar_p3* updates
            if rating_bar_p3.status == NOT_STARTED and t >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                rating_bar_p3.frameNStart = frameN  # exact frame index
                rating_bar_p3.tStart = t  # local t and not account for scr refresh
                rating_bar_p3.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(rating_bar_p3, 'tStartRefresh')  # time at next scr refresh
                rating_bar_p3.setAutoDraw(True)
            continueRoutine &= rating_bar_p3.noResponse  # a response ends the trial
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p3_rating_2Components:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p3_rating_2"-------
        for thisComponent in p3_rating_2Components:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p3_loop.addData('pic_p3.started', pic_p3.tStartRefresh)
        p3_loop.addData('pic_p3.stopped', pic_p3.tStopRefresh)
        # store data for p3_loop (TrialHandler)
        p3_loop.addData('rating_bar_p3.response', rating_bar_p3.getRating())
        p3_loop.addData('rating_bar_p3.rt', rating_bar_p3.getRT())
        p3_loop.addData('rating_bar_p3.started', rating_bar_p3.tStart)
        p3_loop.addData('rating_bar_p3.stopped', rating_bar_p3.tStop)
        # the Routine "p3_rating_2" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        thisExp.nextEntry()
        
    # completed 1 repeats of 'p3_loop'
    
    # get names of stimulus parameters
    if p3_loop.trialList in ([], [None], None):
        params = []
    else:
        params = p3_loop.trialList[0].keys()
    # save data for this loop
    p3_loop.saveAsExcel(filename + '.xlsx', sheetName='p3_loop',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    
    # ------Prepare to start Routine "p3_end"-------
    continueRoutine = True
    # update component parameters for each repeat
    resp2_p3.keys = []
    resp2_p3.rt = []
    _resp2_p3_allKeys = []
    # keep track of which components have finished
    p3_endComponents = [msg2_p3, resp2_p3]
    for thisComponent in p3_endComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    p3_endClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "p3_end"-------
    while continueRoutine:
        # get current time
        t = p3_endClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=p3_endClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *msg2_p3* updates
        if msg2_p3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            msg2_p3.frameNStart = frameN  # exact frame index
            msg2_p3.tStart = t  # local t and not account for scr refresh
            msg2_p3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(msg2_p3, 'tStartRefresh')  # time at next scr refresh
            msg2_p3.setAutoDraw(True)
        
        # *resp2_p3* updates
        waitOnFlip = False
        if resp2_p3.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp2_p3.frameNStart = frameN  # exact frame index
            resp2_p3.tStart = t  # local t and not account for scr refresh
            resp2_p3.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp2_p3, 'tStartRefresh')  # time at next scr refresh
            resp2_p3.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp2_p3.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp2_p3.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp2_p3.status == STARTED and not waitOnFlip:
            theseKeys = resp2_p3.getKeys(keyList=['space', 'right'], waitRelease=False)
            _resp2_p3_allKeys.extend(theseKeys)
            if len(_resp2_p3_allKeys):
                resp2_p3.keys = _resp2_p3_allKeys[-1].name  # just the last key pressed
                resp2_p3.rt = _resp2_p3_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        if resp2_p3.keys == "right":
                p3_repeat.finished = True
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in p3_endComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "p3_end"-------
    for thisComponent in p3_endComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    p3_repeat.addData('msg2_p3.started', msg2_p3.tStartRefresh)
    p3_repeat.addData('msg2_p3.stopped', msg2_p3.tStopRefresh)
    # the Routine "p3_end" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 100 repeats of 'p3_repeat'

# get names of stimulus parameters
if p3_repeat.trialList in ([], [None], None):
    params = []
else:
    params = p3_repeat.trialList[0].keys()
# save data for this loop
p3_repeat.saveAsExcel(filename + '.xlsx', sheetName='p3_repeat',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "p4_instr"-------
continueRoutine = True
# update component parameters for each repeat
resp_p4.keys = []
resp_p4.rt = []
_resp_p4_allKeys = []
# keep track of which components have finished
p4_instrComponents = [msg_p4, resp_p4]
for thisComponent in p4_instrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
p4_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "p4_instr"-------
while continueRoutine:
    # get current time
    t = p4_instrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=p4_instrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_p4* updates
    if msg_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_p4.frameNStart = frameN  # exact frame index
        msg_p4.tStart = t  # local t and not account for scr refresh
        msg_p4.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_p4, 'tStartRefresh')  # time at next scr refresh
        msg_p4.setAutoDraw(True)
    
    # *resp_p4* updates
    waitOnFlip = False
    if resp_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_p4.frameNStart = frameN  # exact frame index
        resp_p4.tStart = t  # local t and not account for scr refresh
        resp_p4.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_p4, 'tStartRefresh')  # time at next scr refresh
        resp_p4.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_p4.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_p4.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_p4.status == STARTED and not waitOnFlip:
        theseKeys = resp_p4.getKeys(keyList=['right'], waitRelease=False)
        _resp_p4_allKeys.extend(theseKeys)
        if len(_resp_p4_allKeys):
            resp_p4.keys = _resp_p4_allKeys[-1].name  # just the last key pressed
            resp_p4.rt = _resp_p4_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in p4_instrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "p4_instr"-------
for thisComponent in p4_instrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_p4.started', msg_p4.tStartRefresh)
thisExp.addData('msg_p4.stopped', msg_p4.tStopRefresh)
# the Routine "p4_instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
p4_repeat = data.TrialHandler(nReps=100, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('p4_repeat.xlsx'),
    seed=None, name='p4_repeat')
thisExp.addLoop(p4_repeat)  # add the loop to the experiment
thisP4_repeat = p4_repeat.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisP4_repeat.rgb)
if thisP4_repeat != None:
    for paramName in thisP4_repeat:
        exec('{} = thisP4_repeat[paramName]'.format(paramName))

for thisP4_repeat in p4_repeat:
    currentLoop = p4_repeat
    # abbreviate parameter names if possible (e.g. rgb = thisP4_repeat.rgb)
    if thisP4_repeat != None:
        for paramName in thisP4_repeat:
            exec('{} = thisP4_repeat[paramName]'.format(paramName))
    
    # set up handler to look after randomisation of conditions etc
    p4_loop = data.TrialHandler(nReps=1, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions(p4_file_name),
        seed=None, name='p4_loop')
    thisExp.addLoop(p4_loop)  # add the loop to the experiment
    thisP4_loop = p4_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisP4_loop.rgb)
    if thisP4_loop != None:
        for paramName in thisP4_loop:
            exec('{} = thisP4_loop[paramName]'.format(paramName))
    
    for thisP4_loop in p4_loop:
        currentLoop = p4_loop
        # abbreviate parameter names if possible (e.g. rgb = thisP4_loop.rgb)
        if thisP4_loop != None:
            for paramName in thisP4_loop:
                exec('{} = thisP4_loop[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "p4_choice"-------
        continueRoutine = True
        # update component parameters for each repeat
        left_option_p4.setImage(p4_pic_left)
        right_option_p4.setImage(p4_pic_right)
        choice_p4.keys = []
        choice_p4.rt = []
        _choice_p4_allKeys = []
        p4_wrong_ms=''
        # keep track of which components have finished
        p4_choiceComponents = [left_option_p4, right_option_p4, fixation_p4, choice_p4, left_gray_p4, right_gray_p4, fc_wrong_p4]
        for thisComponent in p4_choiceComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p4_choiceClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p4_choice"-------
        while continueRoutine:
            # get current time
            t = p4_choiceClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p4_choiceClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *left_option_p4* updates
            if left_option_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                left_option_p4.frameNStart = frameN  # exact frame index
                left_option_p4.tStart = t  # local t and not account for scr refresh
                left_option_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(left_option_p4, 'tStartRefresh')  # time at next scr refresh
                left_option_p4.setAutoDraw(True)
            
            # *right_option_p4* updates
            if right_option_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                right_option_p4.frameNStart = frameN  # exact frame index
                right_option_p4.tStart = t  # local t and not account for scr refresh
                right_option_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(right_option_p4, 'tStartRefresh')  # time at next scr refresh
                right_option_p4.setAutoDraw(True)
            
            # *fixation_p4* updates
            if fixation_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fixation_p4.frameNStart = frameN  # exact frame index
                fixation_p4.tStart = t  # local t and not account for scr refresh
                fixation_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fixation_p4, 'tStartRefresh')  # time at next scr refresh
                fixation_p4.setAutoDraw(True)
            
            # *choice_p4* updates
            waitOnFlip = False
            if choice_p4.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
                # keep track of start time/frame for later
                choice_p4.frameNStart = frameN  # exact frame index
                choice_p4.tStart = t  # local t and not account for scr refresh
                choice_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(choice_p4, 'tStartRefresh')  # time at next scr refresh
                choice_p4.status = STARTED
                # keyboard checking is just starting
                waitOnFlip = True
                win.callOnFlip(choice_p4.clock.reset)  # t=0 on next screen flip
                win.callOnFlip(choice_p4.clearEvents, eventType='keyboard')  # clear events on next screen flip
            if choice_p4.status == STARTED and not waitOnFlip:
                theseKeys = choice_p4.getKeys(keyList=['left', 'right'], waitRelease=False)
                _choice_p4_allKeys.extend(theseKeys)
                if len(_choice_p4_allKeys):
                    choice_p4.keys = _choice_p4_allKeys[-1].name  # just the last key pressed
                    choice_p4.rt = _choice_p4_allKeys[-1].rt
            
            # *left_gray_p4* updates
            if left_gray_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                left_gray_p4.frameNStart = frameN  # exact frame index
                left_gray_p4.tStart = t  # local t and not account for scr refresh
                left_gray_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(left_gray_p4, 'tStartRefresh')  # time at next scr refresh
                left_gray_p4.setAutoDraw(True)
            if left_gray_p4.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > left_gray_p4.tStartRefresh + p4_forced_left*100-frameTolerance:
                    # keep track of stop time/frame for later
                    left_gray_p4.tStop = t  # not accounting for scr refresh
                    left_gray_p4.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(left_gray_p4, 'tStopRefresh')  # time at next scr refresh
                    left_gray_p4.setAutoDraw(False)
            
            # *right_gray_p4* updates
            if right_gray_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                right_gray_p4.frameNStart = frameN  # exact frame index
                right_gray_p4.tStart = t  # local t and not account for scr refresh
                right_gray_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(right_gray_p4, 'tStartRefresh')  # time at next scr refresh
                right_gray_p4.setAutoDraw(True)
            if right_gray_p4.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > right_gray_p4.tStartRefresh + p4_forced_right*100-frameTolerance:
                    # keep track of stop time/frame for later
                    right_gray_p4.tStop = t  # not accounting for scr refresh
                    right_gray_p4.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(right_gray_p4, 'tStopRefresh')  # time at next scr refresh
                    right_gray_p4.setAutoDraw(False)
            if (p4_forced_left and choice_p4.keys=="right") or (p4_forced_right and choice_p4.keys=="left"):
                p4_wrong_ms='Please select the cued option'
            elif (choice_p4.keys=="left") or (choice_p4.keys=="right"):
                continueRoutine = False
            
            # *fc_wrong_p4* updates
            if fc_wrong_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fc_wrong_p4.frameNStart = frameN  # exact frame index
                fc_wrong_p4.tStart = t  # local t and not account for scr refresh
                fc_wrong_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fc_wrong_p4, 'tStartRefresh')  # time at next scr refresh
                fc_wrong_p4.setAutoDraw(True)
            if fc_wrong_p4.status == STARTED:  # only update if drawing
                fc_wrong_p4.setText(p4_wrong_ms, log=False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p4_choiceComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p4_choice"-------
        for thisComponent in p4_choiceComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p4_loop.addData('left_option_p4.started', left_option_p4.tStartRefresh)
        p4_loop.addData('left_option_p4.stopped', left_option_p4.tStopRefresh)
        p4_loop.addData('right_option_p4.started', right_option_p4.tStartRefresh)
        p4_loop.addData('right_option_p4.stopped', right_option_p4.tStopRefresh)
        p4_loop.addData('fixation_p4.started', fixation_p4.tStartRefresh)
        p4_loop.addData('fixation_p4.stopped', fixation_p4.tStopRefresh)
        # check responses
        if choice_p4.keys in ['', [], None]:  # No response was made
            choice_p4.keys = None
        p4_loop.addData('choice_p4.keys',choice_p4.keys)
        if choice_p4.keys != None:  # we had a response
            p4_loop.addData('choice_p4.rt', choice_p4.rt)
        p4_loop.addData('choice_p4.started', choice_p4.tStartRefresh)
        p4_loop.addData('choice_p4.stopped', choice_p4.tStopRefresh)
        p4_loop.addData('left_gray_p4.started', left_gray_p4.tStartRefresh)
        p4_loop.addData('left_gray_p4.stopped', left_gray_p4.tStopRefresh)
        p4_loop.addData('right_gray_p4.started', right_gray_p4.tStartRefresh)
        p4_loop.addData('right_gray_p4.stopped', right_gray_p4.tStopRefresh)
        p4_loop.addData('fc_wrong_p4.started', fc_wrong_p4.tStartRefresh)
        p4_loop.addData('fc_wrong_p4.stopped', fc_wrong_p4.tStopRefresh)
        # the Routine "p4_choice" was not non-slip safe, so reset the non-slip timer
        routineTimer.reset()
        
        # ------Prepare to start Routine "p4_choice_fb"-------
        continueRoutine = True
        routineTimer.add(1.000000)
        # update component parameters for each repeat
        if choice_p4.keys=="left":#stored on last run routine
          msg_p4 = p4_reward_1
        elif choice_p4.keys=="right":
          msg_p4 = p4_reward_2
        fb_p4.setText(msg_p4)
        # keep track of which components have finished
        p4_choice_fbComponents = [fb_p4]
        for thisComponent in p4_choice_fbComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        p4_choice_fbClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "p4_choice_fb"-------
        while continueRoutine and routineTimer.getTime() > 0:
            # get current time
            t = p4_choice_fbClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=p4_choice_fbClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *fb_p4* updates
            if fb_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                fb_p4.frameNStart = frameN  # exact frame index
                fb_p4.tStart = t  # local t and not account for scr refresh
                fb_p4.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(fb_p4, 'tStartRefresh')  # time at next scr refresh
                fb_p4.setAutoDraw(True)
            if fb_p4.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > fb_p4.tStartRefresh + 1-frameTolerance:
                    # keep track of stop time/frame for later
                    fb_p4.tStop = t  # not accounting for scr refresh
                    fb_p4.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(fb_p4, 'tStopRefresh')  # time at next scr refresh
                    fb_p4.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in p4_choice_fbComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "p4_choice_fb"-------
        for thisComponent in p4_choice_fbComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        p4_loop.addData('fb_p4.started', fb_p4.tStartRefresh)
        p4_loop.addData('fb_p4.stopped', fb_p4.tStopRefresh)
        
        # set up handler to look after randomisation of conditions etc
        p4_rating_loop = data.TrialHandler(nReps=1, method='random', 
            extraInfo=expInfo, originPath=-1,
            trialList=data.importConditions('p4_rating_file.xlsx'),
            seed=None, name='p4_rating_loop')
        thisExp.addLoop(p4_rating_loop)  # add the loop to the experiment
        thisP4_rating_loop = p4_rating_loop.trialList[0]  # so we can initialise stimuli with some values
        # abbreviate parameter names if possible (e.g. rgb = thisP4_rating_loop.rgb)
        if thisP4_rating_loop != None:
            for paramName in thisP4_rating_loop:
                exec('{} = thisP4_rating_loop[paramName]'.format(paramName))
        
        for thisP4_rating_loop in p4_rating_loop:
            currentLoop = p4_rating_loop
            # abbreviate parameter names if possible (e.g. rgb = thisP4_rating_loop.rgb)
            if thisP4_rating_loop != None:
                for paramName in thisP4_rating_loop:
                    exec('{} = thisP4_rating_loop[paramName]'.format(paramName))
            
            # ------Prepare to start Routine "p4_rating"-------
            continueRoutine = True
            # update component parameters for each repeat
            rating_p4.setImage(p4_rating_pic)
            rating_bar_p4.reset()
            # skip it if no rating is needed
            if p4_with_rating == 0:
                continueRoutine = False
            # keep track of which components have finished
            p4_ratingComponents = [rating_p4, rating_bar_p4, reminder_rating_p4]
            for thisComponent in p4_ratingComponents:
                thisComponent.tStart = None
                thisComponent.tStop = None
                thisComponent.tStartRefresh = None
                thisComponent.tStopRefresh = None
                if hasattr(thisComponent, 'status'):
                    thisComponent.status = NOT_STARTED
            # reset timers
            t = 0
            _timeToFirstFrame = win.getFutureFlipTime(clock="now")
            p4_ratingClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
            frameN = -1
            
            # -------Run Routine "p4_rating"-------
            while continueRoutine:
                # get current time
                t = p4_ratingClock.getTime()
                tThisFlip = win.getFutureFlipTime(clock=p4_ratingClock)
                tThisFlipGlobal = win.getFutureFlipTime(clock=None)
                frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
                # update/draw components on each frame
                
                # *rating_p4* updates
                if rating_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    rating_p4.frameNStart = frameN  # exact frame index
                    rating_p4.tStart = t  # local t and not account for scr refresh
                    rating_p4.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(rating_p4, 'tStartRefresh')  # time at next scr refresh
                    rating_p4.setAutoDraw(True)
                # *rating_bar_p4* updates
                if rating_bar_p4.status == NOT_STARTED and t >= 0.0-frameTolerance:
                    # keep track of start time/frame for later
                    rating_bar_p4.frameNStart = frameN  # exact frame index
                    rating_bar_p4.tStart = t  # local t and not account for scr refresh
                    rating_bar_p4.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(rating_bar_p4, 'tStartRefresh')  # time at next scr refresh
                    rating_bar_p4.setAutoDraw(True)
                continueRoutine &= rating_bar_p4.noResponse  # a response ends the trial
                
                # *reminder_rating_p4* updates
                if reminder_rating_p4.status == NOT_STARTED and tThisFlip >= 5-frameTolerance:
                    # keep track of start time/frame for later
                    reminder_rating_p4.frameNStart = frameN  # exact frame index
                    reminder_rating_p4.tStart = t  # local t and not account for scr refresh
                    reminder_rating_p4.tStartRefresh = tThisFlipGlobal  # on global time
                    win.timeOnFlip(reminder_rating_p4, 'tStartRefresh')  # time at next scr refresh
                    reminder_rating_p4.setAutoDraw(True)
                
                # check for quit (typically the Esc key)
                if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                    core.quit()
                
                # check if all components have finished
                if not continueRoutine:  # a component has requested a forced-end of Routine
                    break
                continueRoutine = False  # will revert to True if at least one component still running
                for thisComponent in p4_ratingComponents:
                    if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                        continueRoutine = True
                        break  # at least one component has not yet finished
                
                # refresh the screen
                if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                    win.flip()
            
            # -------Ending Routine "p4_rating"-------
            for thisComponent in p4_ratingComponents:
                if hasattr(thisComponent, "setAutoDraw"):
                    thisComponent.setAutoDraw(False)
            p4_rating_loop.addData('rating_p4.started', rating_p4.tStartRefresh)
            p4_rating_loop.addData('rating_p4.stopped', rating_p4.tStopRefresh)
            # store data for p4_rating_loop (TrialHandler)
            p4_rating_loop.addData('rating_bar_p4.response', rating_bar_p4.getRating())
            p4_rating_loop.addData('rating_bar_p4.rt', rating_bar_p4.getRT())
            p4_rating_loop.addData('rating_bar_p4.started', rating_bar_p4.tStart)
            p4_rating_loop.addData('rating_bar_p4.stopped', rating_bar_p4.tStop)
            p4_rating_loop.addData('reminder_rating_p4.started', reminder_rating_p4.tStartRefresh)
            p4_rating_loop.addData('reminder_rating_p4.stopped', reminder_rating_p4.tStopRefresh)
            # the Routine "p4_rating" was not non-slip safe, so reset the non-slip timer
            routineTimer.reset()
            thisExp.nextEntry()
            
        # completed 1 repeats of 'p4_rating_loop'
        
        # get names of stimulus parameters
        if p4_rating_loop.trialList in ([], [None], None):
            params = []
        else:
            params = p4_rating_loop.trialList[0].keys()
        # save data for this loop
        p4_rating_loop.saveAsExcel(filename + '.xlsx', sheetName='p4_rating_loop',
            stimOut=params,
            dataOut=['n','all_mean','all_std', 'all_raw'])
        thisExp.nextEntry()
        
    # completed 1 repeats of 'p4_loop'
    
    # get names of stimulus parameters
    if p4_loop.trialList in ([], [None], None):
        params = []
    else:
        params = p4_loop.trialList[0].keys()
    # save data for this loop
    p4_loop.saveAsExcel(filename + '.xlsx', sheetName='p4_loop',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    
    # ------Prepare to start Routine "p4_end"-------
    continueRoutine = True
    # update component parameters for each repeat
    resp2_p4.keys = []
    resp2_p4.rt = []
    _resp2_p4_allKeys = []
    # keep track of which components have finished
    p4_endComponents = [msg2_p4, resp2_p4]
    for thisComponent in p4_endComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    p4_endClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "p4_end"-------
    while continueRoutine:
        # get current time
        t = p4_endClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=p4_endClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *msg2_p4* updates
        if msg2_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            msg2_p4.frameNStart = frameN  # exact frame index
            msg2_p4.tStart = t  # local t and not account for scr refresh
            msg2_p4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(msg2_p4, 'tStartRefresh')  # time at next scr refresh
            msg2_p4.setAutoDraw(True)
        
        # *resp2_p4* updates
        waitOnFlip = False
        if resp2_p4.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            resp2_p4.frameNStart = frameN  # exact frame index
            resp2_p4.tStart = t  # local t and not account for scr refresh
            resp2_p4.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(resp2_p4, 'tStartRefresh')  # time at next scr refresh
            resp2_p4.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(resp2_p4.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(resp2_p4.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if resp2_p4.status == STARTED and not waitOnFlip:
            theseKeys = resp2_p4.getKeys(keyList=['space', 'right'], waitRelease=False)
            _resp2_p4_allKeys.extend(theseKeys)
            if len(_resp2_p4_allKeys):
                resp2_p4.keys = _resp2_p4_allKeys[-1].name  # just the last key pressed
                resp2_p4.rt = _resp2_p4_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        if resp2_p4.keys == "right":
                p4_repeat.finished = True
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in p4_endComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "p4_end"-------
    for thisComponent in p4_endComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    p4_repeat.addData('msg2_p4.started', msg2_p4.tStartRefresh)
    p4_repeat.addData('msg2_p4.stopped', msg2_p4.tStopRefresh)
    # the Routine "p4_end" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 100 repeats of 'p4_repeat'

# get names of stimulus parameters
if p4_repeat.trialList in ([], [None], None):
    params = []
else:
    params = p4_repeat.trialList[0].keys()
# save data for this loop
p4_repeat.saveAsExcel(filename + '.xlsx', sheetName='p4_repeat',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "test_instr"-------
continueRoutine = True
# update component parameters for each repeat
instr_resp.keys = []
instr_resp.rt = []
_instr_resp_allKeys = []
# determine the order of tasks by parity of the participant id
participant_id = expInfo['participant']
# turn it into integer
participant_id = int(participant_id)
if participant_id % 2 ==0:
    task_order = '1'
else:
    task_order = '2'
# keep track of which components have finished
test_instrComponents = [instr_resp, test_instr_txt]
for thisComponent in test_instrComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
test_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "test_instr"-------
while continueRoutine:
    # get current time
    t = test_instrClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=test_instrClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *instr_resp* updates
    waitOnFlip = False
    if instr_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        instr_resp.frameNStart = frameN  # exact frame index
        instr_resp.tStart = t  # local t and not account for scr refresh
        instr_resp.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(instr_resp, 'tStartRefresh')  # time at next scr refresh
        instr_resp.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(instr_resp.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(instr_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if instr_resp.status == STARTED and not waitOnFlip:
        theseKeys = instr_resp.getKeys(keyList=['right'], waitRelease=False)
        _instr_resp_allKeys.extend(theseKeys)
        if len(_instr_resp_allKeys):
            instr_resp.keys = _instr_resp_allKeys[-1].name  # just the last key pressed
            instr_resp.rt = _instr_resp_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # *test_instr_txt* updates
    if test_instr_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        test_instr_txt.frameNStart = frameN  # exact frame index
        test_instr_txt.tStart = t  # local t and not account for scr refresh
        test_instr_txt.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(test_instr_txt, 'tStartRefresh')  # time at next scr refresh
        test_instr_txt.setAutoDraw(True)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in test_instrComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "test_instr"-------
for thisComponent in test_instrComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
# check responses
if instr_resp.keys in ['', [], None]:  # No response was made
    instr_resp.keys = None
thisExp.addData('instr_resp.keys',instr_resp.keys)
if instr_resp.keys != None:  # we had a response
    thisExp.addData('instr_resp.rt', instr_resp.rt)
thisExp.addData('instr_resp.started', instr_resp.tStartRefresh)
thisExp.addData('instr_resp.stopped', instr_resp.tStopRefresh)
thisExp.nextEntry()
thisExp.addData('test_instr_txt.started', test_instr_txt.tStartRefresh)
thisExp.addData('test_instr_txt.stopped', test_instr_txt.tStopRefresh)
# the Routine "test_instr" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
trials = data.TrialHandler(nReps=1.0, method='sequential', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions("choice_conditions_3arm_rtblock_single"+task_order+".xlsx"),
    seed=None, name='trials')
thisExp.addLoop(trials)  # add the loop to the experiment
thisTrial = trials.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
if thisTrial != None:
    for paramName in thisTrial:
        exec('{} = thisTrial[paramName]'.format(paramName))

for thisTrial in trials:
    currentLoop = trials
    # abbreviate parameter names if possible (e.g. rgb = thisTrial.rgb)
    if thisTrial != None:
        for paramName in thisTrial:
            exec('{} = thisTrial[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "break_bt_sess"-------
    continueRoutine = True
    # update component parameters for each repeat
    break_resp.keys = []
    break_resp.rt = []
    _break_resp_allKeys = []
    # skip it for the 1st session
    if with_block_break == 0:
        continueRoutine = False
    # keep track of which components have finished
    break_bt_sessComponents = [break_text, break_resp]
    for thisComponent in break_bt_sessComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    break_bt_sessClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "break_bt_sess"-------
    while continueRoutine:
        # get current time
        t = break_bt_sessClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=break_bt_sessClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *break_text* updates
        if break_text.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            break_text.frameNStart = frameN  # exact frame index
            break_text.tStart = t  # local t and not account for scr refresh
            break_text.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(break_text, 'tStartRefresh')  # time at next scr refresh
            break_text.setAutoDraw(True)
        
        # *break_resp* updates
        waitOnFlip = False
        if break_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            break_resp.frameNStart = frameN  # exact frame index
            break_resp.tStart = t  # local t and not account for scr refresh
            break_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(break_resp, 'tStartRefresh')  # time at next scr refresh
            break_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(break_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(break_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if break_resp.status == STARTED and not waitOnFlip:
            theseKeys = break_resp.getKeys(keyList=['right'], waitRelease=False)
            _break_resp_allKeys.extend(theseKeys)
            if len(_break_resp_allKeys):
                break_resp.keys = _break_resp_allKeys[-1].name  # just the last key pressed
                break_resp.rt = _break_resp_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in break_bt_sessComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "break_bt_sess"-------
    for thisComponent in break_bt_sessComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('break_text.started', break_text.tStartRefresh)
    trials.addData('break_text.stopped', break_text.tStopRefresh)
    # check responses
    if break_resp.keys in ['', [], None]:  # No response was made
        break_resp.keys = None
    trials.addData('break_resp.keys',break_resp.keys)
    if break_resp.keys != None:  # we had a response
        trials.addData('break_resp.rt', break_resp.rt)
    trials.addData('break_resp.started', break_resp.tStartRefresh)
    trials.addData('break_resp.stopped', break_resp.tStopRefresh)
    # the Routine "break_bt_sess" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "new_instr"-------
    continueRoutine = True
    # update component parameters for each repeat
    new_instr_resp.keys = []
    new_instr_resp.rt = []
    _new_instr_resp_allKeys = []
    # present it only when task changes
    if with_new_instr == 0:
        continueRoutine = False
    # keep track of which components have finished
    new_instrComponents = [new_instr_resp, new_instr_txt]
    for thisComponent in new_instrComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    new_instrClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "new_instr"-------
    while continueRoutine:
        # get current time
        t = new_instrClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=new_instrClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *new_instr_resp* updates
        waitOnFlip = False
        if new_instr_resp.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            new_instr_resp.frameNStart = frameN  # exact frame index
            new_instr_resp.tStart = t  # local t and not account for scr refresh
            new_instr_resp.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(new_instr_resp, 'tStartRefresh')  # time at next scr refresh
            new_instr_resp.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(new_instr_resp.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(new_instr_resp.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if new_instr_resp.status == STARTED and not waitOnFlip:
            theseKeys = new_instr_resp.getKeys(keyList=['right'], waitRelease=False)
            _new_instr_resp_allKeys.extend(theseKeys)
            if len(_new_instr_resp_allKeys):
                new_instr_resp.keys = _new_instr_resp_allKeys[-1].name  # just the last key pressed
                new_instr_resp.rt = _new_instr_resp_allKeys[-1].rt
                # a response ends the routine
                continueRoutine = False
        
        # *new_instr_txt* updates
        if new_instr_txt.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            new_instr_txt.frameNStart = frameN  # exact frame index
            new_instr_txt.tStart = t  # local t and not account for scr refresh
            new_instr_txt.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(new_instr_txt, 'tStartRefresh')  # time at next scr refresh
            new_instr_txt.setAutoDraw(True)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in new_instrComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "new_instr"-------
    for thisComponent in new_instrComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    # check responses
    if new_instr_resp.keys in ['', [], None]:  # No response was made
        new_instr_resp.keys = None
    trials.addData('new_instr_resp.keys',new_instr_resp.keys)
    if new_instr_resp.keys != None:  # we had a response
        trials.addData('new_instr_resp.rt', new_instr_resp.rt)
    trials.addData('new_instr_resp.started', new_instr_resp.tStartRefresh)
    trials.addData('new_instr_resp.stopped', new_instr_resp.tStopRefresh)
    trials.addData('new_instr_txt.started', new_instr_txt.tStartRefresh)
    trials.addData('new_instr_txt.stopped', new_instr_txt.tStopRefresh)
    # the Routine "new_instr" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "test_choice"-------
    continueRoutine = True
    # update component parameters for each repeat
    left_option.setImage(pic_left)
    right_option.setImage(pic_right)
    choice.keys = []
    choice.rt = []
    _choice_allKeys = []
    fc_wrong_ms=''
    # keep track of which components have finished
    test_choiceComponents = [left_option, right_option, fixation, choice, left_gray, right_gray, fc_wrong]
    for thisComponent in test_choiceComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    test_choiceClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "test_choice"-------
    while continueRoutine:
        # get current time
        t = test_choiceClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=test_choiceClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *left_option* updates
        if left_option.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            left_option.frameNStart = frameN  # exact frame index
            left_option.tStart = t  # local t and not account for scr refresh
            left_option.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(left_option, 'tStartRefresh')  # time at next scr refresh
            left_option.setAutoDraw(True)
        
        # *right_option* updates
        if right_option.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            right_option.frameNStart = frameN  # exact frame index
            right_option.tStart = t  # local t and not account for scr refresh
            right_option.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(right_option, 'tStartRefresh')  # time at next scr refresh
            right_option.setAutoDraw(True)
        
        # *fixation* updates
        if fixation.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fixation.frameNStart = frameN  # exact frame index
            fixation.tStart = t  # local t and not account for scr refresh
            fixation.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fixation, 'tStartRefresh')  # time at next scr refresh
            fixation.setAutoDraw(True)
        
        # *choice* updates
        waitOnFlip = False
        if choice.status == NOT_STARTED and tThisFlip >= 0-frameTolerance:
            # keep track of start time/frame for later
            choice.frameNStart = frameN  # exact frame index
            choice.tStart = t  # local t and not account for scr refresh
            choice.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(choice, 'tStartRefresh')  # time at next scr refresh
            choice.status = STARTED
            # keyboard checking is just starting
            waitOnFlip = True
            win.callOnFlip(choice.clock.reset)  # t=0 on next screen flip
            win.callOnFlip(choice.clearEvents, eventType='keyboard')  # clear events on next screen flip
        if choice.status == STARTED and not waitOnFlip:
            theseKeys = choice.getKeys(keyList=['left', 'right'], waitRelease=False)
            _choice_allKeys.extend(theseKeys)
            if len(_choice_allKeys):
                choice.keys = _choice_allKeys[-1].name  # just the last key pressed
                choice.rt = _choice_allKeys[-1].rt
                # was this correct?
                if (choice.keys == str('')) or (choice.keys == ''):
                    choice.corr = 1
                else:
                    choice.corr = 0
        
        # *left_gray* updates
        if left_gray.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            left_gray.frameNStart = frameN  # exact frame index
            left_gray.tStart = t  # local t and not account for scr refresh
            left_gray.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(left_gray, 'tStartRefresh')  # time at next scr refresh
            left_gray.setAutoDraw(True)
        if left_gray.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > left_gray.tStartRefresh + forced_left*100-frameTolerance:
                # keep track of stop time/frame for later
                left_gray.tStop = t  # not accounting for scr refresh
                left_gray.frameNStop = frameN  # exact frame index
                win.timeOnFlip(left_gray, 'tStopRefresh')  # time at next scr refresh
                left_gray.setAutoDraw(False)
        
        # *right_gray* updates
        if right_gray.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            right_gray.frameNStart = frameN  # exact frame index
            right_gray.tStart = t  # local t and not account for scr refresh
            right_gray.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(right_gray, 'tStartRefresh')  # time at next scr refresh
            right_gray.setAutoDraw(True)
        if right_gray.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > right_gray.tStartRefresh + forced_right*100-frameTolerance:
                # keep track of stop time/frame for later
                right_gray.tStop = t  # not accounting for scr refresh
                right_gray.frameNStop = frameN  # exact frame index
                win.timeOnFlip(right_gray, 'tStopRefresh')  # time at next scr refresh
                right_gray.setAutoDraw(False)
        if t >= c_durat:
            continueRoutine = False
        elif (forced_left and choice.keys=="right") or (forced_right and choice.keys=="left"):
            fc_wrong_ms='Please select the cued option'
        elif (choice.keys=="left") or (choice.keys=="right"):
            continueRoutine = False
        
        # *fc_wrong* updates
        if fc_wrong.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            fc_wrong.frameNStart = frameN  # exact frame index
            fc_wrong.tStart = t  # local t and not account for scr refresh
            fc_wrong.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(fc_wrong, 'tStartRefresh')  # time at next scr refresh
            fc_wrong.setAutoDraw(True)
        if fc_wrong.status == STARTED:  # only update if drawing
            fc_wrong.setText(fc_wrong_ms, log=False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in test_choiceComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "test_choice"-------
    for thisComponent in test_choiceComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('left_option.started', left_option.tStartRefresh)
    trials.addData('left_option.stopped', left_option.tStopRefresh)
    trials.addData('right_option.started', right_option.tStartRefresh)
    trials.addData('right_option.stopped', right_option.tStopRefresh)
    trials.addData('fixation.started', fixation.tStartRefresh)
    trials.addData('fixation.stopped', fixation.tStopRefresh)
    # check responses
    if choice.keys in ['', [], None]:  # No response was made
        choice.keys = None
        # was no response the correct answer?!
        if str('').lower() == 'none':
           choice.corr = 1;  # correct non-response
        else:
           choice.corr = 0;  # failed to respond (incorrectly)
    # store data for trials (TrialHandler)
    trials.addData('choice.keys',choice.keys)
    trials.addData('choice.corr', choice.corr)
    if choice.keys != None:  # we had a response
        trials.addData('choice.rt', choice.rt)
    trials.addData('choice.started', choice.tStartRefresh)
    trials.addData('choice.stopped', choice.tStopRefresh)
    trials.addData('left_gray.started', left_gray.tStartRefresh)
    trials.addData('left_gray.stopped', left_gray.tStopRefresh)
    trials.addData('right_gray.started', right_gray.tStartRefresh)
    trials.addData('right_gray.stopped', right_gray.tStopRefresh)
    trials.addData('fc_wrong.started', fc_wrong.tStartRefresh)
    trials.addData('fc_wrong.stopped', fc_wrong.tStopRefresh)
    # the Routine "test_choice" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    
    # ------Prepare to start Routine "choice_fb"-------
    continueRoutine = True
    routineTimer.add(1.000000)
    # update component parameters for each repeat
    if (forced_left and choice.keys=="right") or (forced_right and choice.keys=="left"):
      msg='Wrong choice'
      thisExp.addData('chosen_op',wr_resp)
    elif choice.keys=="left":#stored on last run routine
      msg=reward_stim_1
      thisExp.addData('chosen_op',option_left)
    elif choice.keys=="right":
      msg=reward_stim_2
      thisExp.addData('chosen_op',option_right)
    else:# if no key was pressed
      msg='No response'
      thisExp.addData('chosen_op',no_resp)
    feedback_2.setText(msg)
    # keep track of which components have finished
    choice_fbComponents = [feedback_2]
    for thisComponent in choice_fbComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    choice_fbClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "choice_fb"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = choice_fbClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=choice_fbClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *feedback_2* updates
        if feedback_2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            feedback_2.frameNStart = frameN  # exact frame index
            feedback_2.tStart = t  # local t and not account for scr refresh
            feedback_2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(feedback_2, 'tStartRefresh')  # time at next scr refresh
            feedback_2.setAutoDraw(True)
        if feedback_2.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > feedback_2.tStartRefresh + 1-frameTolerance:
                # keep track of stop time/frame for later
                feedback_2.tStop = t  # not accounting for scr refresh
                feedback_2.frameNStop = frameN  # exact frame index
                win.timeOnFlip(feedback_2, 'tStopRefresh')  # time at next scr refresh
                feedback_2.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in choice_fbComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "choice_fb"-------
    for thisComponent in choice_fbComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('feedback_2.started', feedback_2.tStartRefresh)
    trials.addData('feedback_2.stopped', feedback_2.tStopRefresh)
    
    # ------Prepare to start Routine "ITI"-------
    continueRoutine = True
    routineTimer.add(0.500000)
    # update component parameters for each repeat
    # keep track of which components have finished
    ITIComponents = [iti_fixation]
    for thisComponent in ITIComponents:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    ITIClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "ITI"-------
    while continueRoutine and routineTimer.getTime() > 0:
        # get current time
        t = ITIClock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=ITIClock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *iti_fixation* updates
        if iti_fixation.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            iti_fixation.frameNStart = frameN  # exact frame index
            iti_fixation.tStart = t  # local t and not account for scr refresh
            iti_fixation.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(iti_fixation, 'tStartRefresh')  # time at next scr refresh
            iti_fixation.setAutoDraw(True)
        if iti_fixation.status == STARTED:
            # is it time to stop? (based on global clock, using actual start)
            if tThisFlipGlobal > iti_fixation.tStartRefresh + 0.5-frameTolerance:
                # keep track of stop time/frame for later
                iti_fixation.tStop = t  # not accounting for scr refresh
                iti_fixation.frameNStop = frameN  # exact frame index
                win.timeOnFlip(iti_fixation, 'tStopRefresh')  # time at next scr refresh
                iti_fixation.setAutoDraw(False)
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in ITIComponents:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "ITI"-------
    for thisComponent in ITIComponents:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    trials.addData('iti_fixation.started', iti_fixation.tStartRefresh)
    trials.addData('iti_fixation.stopped', iti_fixation.tStopRefresh)
    
    # set up handler to look after randomisation of conditions etc
    test_rating_loop = data.TrialHandler(nReps=1, method='random', 
        extraInfo=expInfo, originPath=-1,
        trialList=data.importConditions('test_rating_file.xlsx'),
        seed=None, name='test_rating_loop')
    thisExp.addLoop(test_rating_loop)  # add the loop to the experiment
    thisTest_rating_loop = test_rating_loop.trialList[0]  # so we can initialise stimuli with some values
    # abbreviate parameter names if possible (e.g. rgb = thisTest_rating_loop.rgb)
    if thisTest_rating_loop != None:
        for paramName in thisTest_rating_loop:
            exec('{} = thisTest_rating_loop[paramName]'.format(paramName))
    
    for thisTest_rating_loop in test_rating_loop:
        currentLoop = test_rating_loop
        # abbreviate parameter names if possible (e.g. rgb = thisTest_rating_loop.rgb)
        if thisTest_rating_loop != None:
            for paramName in thisTest_rating_loop:
                exec('{} = thisTest_rating_loop[paramName]'.format(paramName))
        
        # ------Prepare to start Routine "test_rating"-------
        continueRoutine = True
        routineTimer.add(20.000000)
        # update component parameters for each repeat
        option_rating1.setImage(test_rating_pic)
        rating_bar1.reset()
        # skip it if no rating is needed
        if with_rating == 0:
            continueRoutine = False
        # keep track of which components have finished
        test_ratingComponents = [option_rating1, rating_bar1, reminder_rating1]
        for thisComponent in test_ratingComponents:
            thisComponent.tStart = None
            thisComponent.tStop = None
            thisComponent.tStartRefresh = None
            thisComponent.tStopRefresh = None
            if hasattr(thisComponent, 'status'):
                thisComponent.status = NOT_STARTED
        # reset timers
        t = 0
        _timeToFirstFrame = win.getFutureFlipTime(clock="now")
        test_ratingClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
        frameN = -1
        
        # -------Run Routine "test_rating"-------
        while continueRoutine and routineTimer.getTime() > 0:
            # get current time
            t = test_ratingClock.getTime()
            tThisFlip = win.getFutureFlipTime(clock=test_ratingClock)
            tThisFlipGlobal = win.getFutureFlipTime(clock=None)
            frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
            # update/draw components on each frame
            
            # *option_rating1* updates
            if option_rating1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                option_rating1.frameNStart = frameN  # exact frame index
                option_rating1.tStart = t  # local t and not account for scr refresh
                option_rating1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(option_rating1, 'tStartRefresh')  # time at next scr refresh
                option_rating1.setAutoDraw(True)
            if option_rating1.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > option_rating1.tStartRefresh + 20-frameTolerance:
                    # keep track of stop time/frame for later
                    option_rating1.tStop = t  # not accounting for scr refresh
                    option_rating1.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(option_rating1, 'tStopRefresh')  # time at next scr refresh
                    option_rating1.setAutoDraw(False)
            # *rating_bar1* updates
            if rating_bar1.status == NOT_STARTED and t >= 0.0-frameTolerance:
                # keep track of start time/frame for later
                rating_bar1.frameNStart = frameN  # exact frame index
                rating_bar1.tStart = t  # local t and not account for scr refresh
                rating_bar1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(rating_bar1, 'tStartRefresh')  # time at next scr refresh
                rating_bar1.setAutoDraw(True)
            continueRoutine &= rating_bar1.noResponse  # a response ends the trial
            
            # *reminder_rating1* updates
            if reminder_rating1.status == NOT_STARTED and tThisFlip >= 5-frameTolerance:
                # keep track of start time/frame for later
                reminder_rating1.frameNStart = frameN  # exact frame index
                reminder_rating1.tStart = t  # local t and not account for scr refresh
                reminder_rating1.tStartRefresh = tThisFlipGlobal  # on global time
                win.timeOnFlip(reminder_rating1, 'tStartRefresh')  # time at next scr refresh
                reminder_rating1.setAutoDraw(True)
            if reminder_rating1.status == STARTED:
                # is it time to stop? (based on global clock, using actual start)
                if tThisFlipGlobal > reminder_rating1.tStartRefresh + 15-frameTolerance:
                    # keep track of stop time/frame for later
                    reminder_rating1.tStop = t  # not accounting for scr refresh
                    reminder_rating1.frameNStop = frameN  # exact frame index
                    win.timeOnFlip(reminder_rating1, 'tStopRefresh')  # time at next scr refresh
                    reminder_rating1.setAutoDraw(False)
            
            # check for quit (typically the Esc key)
            if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
                core.quit()
            
            # check if all components have finished
            if not continueRoutine:  # a component has requested a forced-end of Routine
                break
            continueRoutine = False  # will revert to True if at least one component still running
            for thisComponent in test_ratingComponents:
                if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                    continueRoutine = True
                    break  # at least one component has not yet finished
            
            # refresh the screen
            if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
                win.flip()
        
        # -------Ending Routine "test_rating"-------
        for thisComponent in test_ratingComponents:
            if hasattr(thisComponent, "setAutoDraw"):
                thisComponent.setAutoDraw(False)
        test_rating_loop.addData('option_rating1.started', option_rating1.tStartRefresh)
        test_rating_loop.addData('option_rating1.stopped', option_rating1.tStopRefresh)
        # store data for test_rating_loop (TrialHandler)
        test_rating_loop.addData('rating_bar1.response', rating_bar1.getRating())
        test_rating_loop.addData('rating_bar1.rt', rating_bar1.getRT())
        test_rating_loop.addData('rating_bar1.started', rating_bar1.tStart)
        test_rating_loop.addData('rating_bar1.stopped', rating_bar1.tStop)
        if with_rating == 1:
            thisExp.addData('rt_score1',rating_bar1.getRating()*10)
        else:
            thisExp.addData('rt_score1',no_resp)
        test_rating_loop.addData('reminder_rating1.started', reminder_rating1.tStartRefresh)
        test_rating_loop.addData('reminder_rating1.stopped', reminder_rating1.tStopRefresh)
        thisExp.nextEntry()
        
    # completed 1 repeats of 'test_rating_loop'
    
    # get names of stimulus parameters
    if test_rating_loop.trialList in ([], [None], None):
        params = []
    else:
        params = test_rating_loop.trialList[0].keys()
    # save data for this loop
    test_rating_loop.saveAsExcel(filename + '.xlsx', sheetName='test_rating_loop',
        stimOut=params,
        dataOut=['n','all_mean','all_std', 'all_raw'])
    thisExp.nextEntry()
    
# completed 1.0 repeats of 'trials'

# get names of stimulus parameters
if trials.trialList in ([], [None], None):
    params = []
else:
    params = trials.trialList[0].keys()
# save data for this loop
trials.saveAsExcel(filename + '.xlsx', sheetName='trials',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "post_exp_instr1"-------
continueRoutine = True
# update component parameters for each repeat
resp_post1.keys = []
resp_post1.rt = []
_resp_post1_allKeys = []
# keep track of which components have finished
post_exp_instr1Components = [msg_post1, resp_post1]
for thisComponent in post_exp_instr1Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
post_exp_instr1Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "post_exp_instr1"-------
while continueRoutine:
    # get current time
    t = post_exp_instr1Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=post_exp_instr1Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_post1* updates
    if msg_post1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_post1.frameNStart = frameN  # exact frame index
        msg_post1.tStart = t  # local t and not account for scr refresh
        msg_post1.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_post1, 'tStartRefresh')  # time at next scr refresh
        msg_post1.setAutoDraw(True)
    
    # *resp_post1* updates
    waitOnFlip = False
    if resp_post1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_post1.frameNStart = frameN  # exact frame index
        resp_post1.tStart = t  # local t and not account for scr refresh
        resp_post1.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_post1, 'tStartRefresh')  # time at next scr refresh
        resp_post1.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_post1.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_post1.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_post1.status == STARTED and not waitOnFlip:
        theseKeys = resp_post1.getKeys(keyList=['right'], waitRelease=False)
        _resp_post1_allKeys.extend(theseKeys)
        if len(_resp_post1_allKeys):
            resp_post1.keys = _resp_post1_allKeys[-1].name  # just the last key pressed
            resp_post1.rt = _resp_post1_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in post_exp_instr1Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "post_exp_instr1"-------
for thisComponent in post_exp_instr1Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_post1.started', msg_post1.tStartRefresh)
thisExp.addData('msg_post1.stopped', msg_post1.tStopRefresh)
# the Routine "post_exp_instr1" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
post_rating_loop1 = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('post_rating_file1.xlsx'),
    seed=None, name='post_rating_loop1')
thisExp.addLoop(post_rating_loop1)  # add the loop to the experiment
thisPost_rating_loop1 = post_rating_loop1.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisPost_rating_loop1.rgb)
if thisPost_rating_loop1 != None:
    for paramName in thisPost_rating_loop1:
        exec('{} = thisPost_rating_loop1[paramName]'.format(paramName))

for thisPost_rating_loop1 in post_rating_loop1:
    currentLoop = post_rating_loop1
    # abbreviate parameter names if possible (e.g. rgb = thisPost_rating_loop1.rgb)
    if thisPost_rating_loop1 != None:
        for paramName in thisPost_rating_loop1:
            exec('{} = thisPost_rating_loop1[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "post_exp_rating1"-------
    continueRoutine = True
    # update component parameters for each repeat
    pic_post1.setImage(post_rating_pic1)
    rating_bar_post1.reset()
    # keep track of which components have finished
    post_exp_rating1Components = [pic_post1, rating_bar_post1]
    for thisComponent in post_exp_rating1Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    post_exp_rating1Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "post_exp_rating1"-------
    while continueRoutine:
        # get current time
        t = post_exp_rating1Clock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=post_exp_rating1Clock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *pic_post1* updates
        if pic_post1.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            pic_post1.frameNStart = frameN  # exact frame index
            pic_post1.tStart = t  # local t and not account for scr refresh
            pic_post1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(pic_post1, 'tStartRefresh')  # time at next scr refresh
            pic_post1.setAutoDraw(True)
        # *rating_bar_post1* updates
        if rating_bar_post1.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            rating_bar_post1.frameNStart = frameN  # exact frame index
            rating_bar_post1.tStart = t  # local t and not account for scr refresh
            rating_bar_post1.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(rating_bar_post1, 'tStartRefresh')  # time at next scr refresh
            rating_bar_post1.setAutoDraw(True)
        continueRoutine &= rating_bar_post1.noResponse  # a response ends the trial
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in post_exp_rating1Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "post_exp_rating1"-------
    for thisComponent in post_exp_rating1Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    post_rating_loop1.addData('pic_post1.started', pic_post1.tStartRefresh)
    post_rating_loop1.addData('pic_post1.stopped', pic_post1.tStopRefresh)
    # store data for post_rating_loop1 (TrialHandler)
    post_rating_loop1.addData('rating_bar_post1.response', rating_bar_post1.getRating())
    post_rating_loop1.addData('rating_bar_post1.rt', rating_bar_post1.getRT())
    post_rating_loop1.addData('rating_bar_post1.started', rating_bar_post1.tStart)
    post_rating_loop1.addData('rating_bar_post1.stopped', rating_bar_post1.tStop)
    # the Routine "post_exp_rating1" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'post_rating_loop1'

# get names of stimulus parameters
if post_rating_loop1.trialList in ([], [None], None):
    params = []
else:
    params = post_rating_loop1.trialList[0].keys()
# save data for this loop
post_rating_loop1.saveAsExcel(filename + '.xlsx', sheetName='post_rating_loop1',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "post_exp_instr2"-------
continueRoutine = True
# update component parameters for each repeat
resp_post2.keys = []
resp_post2.rt = []
_resp_post2_allKeys = []
# keep track of which components have finished
post_exp_instr2Components = [msg_post2, resp_post2]
for thisComponent in post_exp_instr2Components:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
post_exp_instr2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "post_exp_instr2"-------
while continueRoutine:
    # get current time
    t = post_exp_instr2Clock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=post_exp_instr2Clock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *msg_post2* updates
    if msg_post2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        msg_post2.frameNStart = frameN  # exact frame index
        msg_post2.tStart = t  # local t and not account for scr refresh
        msg_post2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(msg_post2, 'tStartRefresh')  # time at next scr refresh
        msg_post2.setAutoDraw(True)
    
    # *resp_post2* updates
    waitOnFlip = False
    if resp_post2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        resp_post2.frameNStart = frameN  # exact frame index
        resp_post2.tStart = t  # local t and not account for scr refresh
        resp_post2.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(resp_post2, 'tStartRefresh')  # time at next scr refresh
        resp_post2.status = STARTED
        # keyboard checking is just starting
        waitOnFlip = True
        win.callOnFlip(resp_post2.clock.reset)  # t=0 on next screen flip
        win.callOnFlip(resp_post2.clearEvents, eventType='keyboard')  # clear events on next screen flip
    if resp_post2.status == STARTED and not waitOnFlip:
        theseKeys = resp_post2.getKeys(keyList=['right'], waitRelease=False)
        _resp_post2_allKeys.extend(theseKeys)
        if len(_resp_post2_allKeys):
            resp_post2.keys = _resp_post2_allKeys[-1].name  # just the last key pressed
            resp_post2.rt = _resp_post2_allKeys[-1].rt
            # a response ends the routine
            continueRoutine = False
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in post_exp_instr2Components:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "post_exp_instr2"-------
for thisComponent in post_exp_instr2Components:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('msg_post2.started', msg_post2.tStartRefresh)
thisExp.addData('msg_post2.stopped', msg_post2.tStopRefresh)
# the Routine "post_exp_instr2" was not non-slip safe, so reset the non-slip timer
routineTimer.reset()

# set up handler to look after randomisation of conditions etc
post_rating_loop2 = data.TrialHandler(nReps=1, method='random', 
    extraInfo=expInfo, originPath=-1,
    trialList=data.importConditions('post_rating_file2.xlsx'),
    seed=None, name='post_rating_loop2')
thisExp.addLoop(post_rating_loop2)  # add the loop to the experiment
thisPost_rating_loop2 = post_rating_loop2.trialList[0]  # so we can initialise stimuli with some values
# abbreviate parameter names if possible (e.g. rgb = thisPost_rating_loop2.rgb)
if thisPost_rating_loop2 != None:
    for paramName in thisPost_rating_loop2:
        exec('{} = thisPost_rating_loop2[paramName]'.format(paramName))

for thisPost_rating_loop2 in post_rating_loop2:
    currentLoop = post_rating_loop2
    # abbreviate parameter names if possible (e.g. rgb = thisPost_rating_loop2.rgb)
    if thisPost_rating_loop2 != None:
        for paramName in thisPost_rating_loop2:
            exec('{} = thisPost_rating_loop2[paramName]'.format(paramName))
    
    # ------Prepare to start Routine "post_exp_rating2"-------
    continueRoutine = True
    # update component parameters for each repeat
    pic_post2.setImage(post_rating_pic2)
    rating_bar_post2.reset()
    # keep track of which components have finished
    post_exp_rating2Components = [pic_post2, rating_bar_post2]
    for thisComponent in post_exp_rating2Components:
        thisComponent.tStart = None
        thisComponent.tStop = None
        thisComponent.tStartRefresh = None
        thisComponent.tStopRefresh = None
        if hasattr(thisComponent, 'status'):
            thisComponent.status = NOT_STARTED
    # reset timers
    t = 0
    _timeToFirstFrame = win.getFutureFlipTime(clock="now")
    post_exp_rating2Clock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
    frameN = -1
    
    # -------Run Routine "post_exp_rating2"-------
    while continueRoutine:
        # get current time
        t = post_exp_rating2Clock.getTime()
        tThisFlip = win.getFutureFlipTime(clock=post_exp_rating2Clock)
        tThisFlipGlobal = win.getFutureFlipTime(clock=None)
        frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
        # update/draw components on each frame
        
        # *pic_post2* updates
        if pic_post2.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            pic_post2.frameNStart = frameN  # exact frame index
            pic_post2.tStart = t  # local t and not account for scr refresh
            pic_post2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(pic_post2, 'tStartRefresh')  # time at next scr refresh
            pic_post2.setAutoDraw(True)
        # *rating_bar_post2* updates
        if rating_bar_post2.status == NOT_STARTED and t >= 0.0-frameTolerance:
            # keep track of start time/frame for later
            rating_bar_post2.frameNStart = frameN  # exact frame index
            rating_bar_post2.tStart = t  # local t and not account for scr refresh
            rating_bar_post2.tStartRefresh = tThisFlipGlobal  # on global time
            win.timeOnFlip(rating_bar_post2, 'tStartRefresh')  # time at next scr refresh
            rating_bar_post2.setAutoDraw(True)
        continueRoutine &= rating_bar_post2.noResponse  # a response ends the trial
        
        # check for quit (typically the Esc key)
        if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
            core.quit()
        
        # check if all components have finished
        if not continueRoutine:  # a component has requested a forced-end of Routine
            break
        continueRoutine = False  # will revert to True if at least one component still running
        for thisComponent in post_exp_rating2Components:
            if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
                continueRoutine = True
                break  # at least one component has not yet finished
        
        # refresh the screen
        if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
            win.flip()
    
    # -------Ending Routine "post_exp_rating2"-------
    for thisComponent in post_exp_rating2Components:
        if hasattr(thisComponent, "setAutoDraw"):
            thisComponent.setAutoDraw(False)
    post_rating_loop2.addData('pic_post2.started', pic_post2.tStartRefresh)
    post_rating_loop2.addData('pic_post2.stopped', pic_post2.tStopRefresh)
    # store data for post_rating_loop2 (TrialHandler)
    post_rating_loop2.addData('rating_bar_post2.response', rating_bar_post2.getRating())
    post_rating_loop2.addData('rating_bar_post2.rt', rating_bar_post2.getRT())
    post_rating_loop2.addData('rating_bar_post2.started', rating_bar_post2.tStart)
    post_rating_loop2.addData('rating_bar_post2.stopped', rating_bar_post2.tStop)
    # the Routine "post_exp_rating2" was not non-slip safe, so reset the non-slip timer
    routineTimer.reset()
    thisExp.nextEntry()
    
# completed 1 repeats of 'post_rating_loop2'

# get names of stimulus parameters
if post_rating_loop2.trialList in ([], [None], None):
    params = []
else:
    params = post_rating_loop2.trialList[0].keys()
# save data for this loop
post_rating_loop2.saveAsExcel(filename + '.xlsx', sheetName='post_rating_loop2',
    stimOut=params,
    dataOut=['n','all_mean','all_std', 'all_raw'])

# ------Prepare to start Routine "end"-------
continueRoutine = True
routineTimer.add(3.000000)
# update component parameters for each repeat
# keep track of which components have finished
endComponents = [end_msg]
for thisComponent in endComponents:
    thisComponent.tStart = None
    thisComponent.tStop = None
    thisComponent.tStartRefresh = None
    thisComponent.tStopRefresh = None
    if hasattr(thisComponent, 'status'):
        thisComponent.status = NOT_STARTED
# reset timers
t = 0
_timeToFirstFrame = win.getFutureFlipTime(clock="now")
endClock.reset(-_timeToFirstFrame)  # t0 is time of first possible flip
frameN = -1

# -------Run Routine "end"-------
while continueRoutine and routineTimer.getTime() > 0:
    # get current time
    t = endClock.getTime()
    tThisFlip = win.getFutureFlipTime(clock=endClock)
    tThisFlipGlobal = win.getFutureFlipTime(clock=None)
    frameN = frameN + 1  # number of completed frames (so 0 is the first frame)
    # update/draw components on each frame
    
    # *end_msg* updates
    if end_msg.status == NOT_STARTED and tThisFlip >= 0.0-frameTolerance:
        # keep track of start time/frame for later
        end_msg.frameNStart = frameN  # exact frame index
        end_msg.tStart = t  # local t and not account for scr refresh
        end_msg.tStartRefresh = tThisFlipGlobal  # on global time
        win.timeOnFlip(end_msg, 'tStartRefresh')  # time at next scr refresh
        end_msg.setAutoDraw(True)
    if end_msg.status == STARTED:
        # is it time to stop? (based on global clock, using actual start)
        if tThisFlipGlobal > end_msg.tStartRefresh + 3-frameTolerance:
            # keep track of stop time/frame for later
            end_msg.tStop = t  # not accounting for scr refresh
            end_msg.frameNStop = frameN  # exact frame index
            win.timeOnFlip(end_msg, 'tStopRefresh')  # time at next scr refresh
            end_msg.setAutoDraw(False)
    
    # check for quit (typically the Esc key)
    if endExpNow or defaultKeyboard.getKeys(keyList=["escape"]):
        core.quit()
    
    # check if all components have finished
    if not continueRoutine:  # a component has requested a forced-end of Routine
        break
    continueRoutine = False  # will revert to True if at least one component still running
    for thisComponent in endComponents:
        if hasattr(thisComponent, "status") and thisComponent.status != FINISHED:
            continueRoutine = True
            break  # at least one component has not yet finished
    
    # refresh the screen
    if continueRoutine:  # don't flip if this routine is over or we'll get a blank screen
        win.flip()

# -------Ending Routine "end"-------
for thisComponent in endComponents:
    if hasattr(thisComponent, "setAutoDraw"):
        thisComponent.setAutoDraw(False)
thisExp.addData('end_msg.started', end_msg.tStartRefresh)
thisExp.addData('end_msg.stopped', end_msg.tStopRefresh)
thisExp.saveAsWideText(filename+'.tsv')

# Flip one final time so any remaining win.callOnFlip() 
# and win.timeOnFlip() tasks get executed before quitting
win.flip()

# these shouldn't be strictly necessary (should auto-save)
thisExp.saveAsWideText(filename+'.csv')
thisExp.saveAsPickle(filename)
logging.flush()
# make sure everything is closed down
thisExp.abort()  # or data files will save again on exit
win.close()
core.quit()
