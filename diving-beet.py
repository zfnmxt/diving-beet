#!/usr/bin/env python

import game
import numpy
import pygame
import time
import sys

fullscreen = False

if fullscreen:
    desired_size = None
else:
    desired_size = (1200,800)

pygame.init()
pygame.display.set_caption('Diving Beet')
if desired_size != None:
    screen = pygame.display.set_mode(desired_size)
else:
    screen = pygame.display.set_mode()
width = screen.get_width()
height = screen.get_height()
size = (width,height)
font = pygame.font.Font(None, 36)
pygame.key.set_repeat(100, 100)

def showText(what, where):
    text = font.render(what, 1, (255, 255, 255))
    screen.blit(text, where)

insertable = [ ('lava', 27),
               ('oil', 6),
               ('metal', 26),
               ('plant', 24),
               ('salt water', 8),
               ('salt', 10),
               ('sand', 9),
               ('spout', 25),
               ('stone', 11),
               ('torch', 23),
               ('wall', 29),
               ('water', 7)]

num_insertable = len(insertable)

selection = 6 # sand is selected initially

modify_radius = 5

beet = game.game()
beet_state = beet.new_game(height, width)

while True:
    start = time.time()
    beet_state = beet.step_game(*beet_state)
    frame = beet.render(*beet_state).get()
    end = time.time()
    futhark_time = (end-start)*1000

    start = time.time()
    pygame.surfarray.blit_array(screen, frame)
    end = time.time()
    blit_time = (end-start)*1000

    speedmessage = "Futhark call took %.2fms; blitting %.2fms" % \
                   (futhark_time, blit_time)
    showText(speedmessage, (10, 10))

    showText(insertable[selection][0], (10,30))

    pygame.display.flip()

    for event in pygame.event.get():
        if event.type == pygame.QUIT:
            sys.exit()
        elif event.type == pygame.MOUSEBUTTONDOWN:
            if event.button == 4:
                selection = (selection - 1) % num_insertable
            if event.button == 5:
                selection = (selection + 1) % num_insertable
        if event.type == pygame.KEYDOWN:
            if event.key == pygame.K_RIGHT:
                selection = (selection + 1) % num_insertable
            if event.key == pygame.K_LEFT:
                selection = (selection - 1) % num_insertable
        if pygame.mouse.get_pressed()[0] and pygame.mouse.get_pos() != None:
            # insert the selected element here.
            args = beet_state + pygame.mouse.get_pos() + (modify_radius, insertable[selection][1])
            beet_state = beet.add_element(*args)
        if pygame.mouse.get_pressed()[2] and pygame.mouse.get_pos() != None:
            # remove any element element here.
            args = beet_state + pygame.mouse.get_pos() + (modify_radius, 0)
            beet_state = beet.add_element(*args)

