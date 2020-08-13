#!/usr/bin/env python3
# encoding: utf-8

import sys
import os.path
import argparse
import time
import math
import pygame

def art(*paths):
    return os.path.join(os.path.dirname(__file__), 'art', *paths)

class ExitGame(Exception):
    pass

class FinishLevel(Exception):
    pass

class Namespace(object):
    def __init__(self, **kwargs):
        for k, v in kwargs.items():
            self.__setattr__(k, v)

class Game:
    def __init__(self, scale, fps):
        self.scale = scale
        self.fps = fps
        self.width = 500
        self.height = 200
        self.size_base = (self.width, self.height)
        self.size_scaled = (self.width * self.scale,
                            self.height * self.scale)

    def run(self):
        try:
            self.setup()
            self.load_audio()
            self.load_characters()
            self.load_sprite()
            self.load_objects()
            self.start_screen()
            self.level1()
            self.level2()
        except ExitGame:
            return 0

    def setup(self):
        pygame.mixer.pre_init(44100, -16, 2, 1024)
        pygame.init()
        pygame.display.set_caption('Lemur Lyn: Fare i Fartby')
        self.screen_base = pygame.surface.Surface(self.size_base)
        self.screen_scaled = pygame.display.set_mode(self.size_scaled)
        self.font_large = pygame.font.Font(
            art('fonts', 'lmromandunh', 'lmromandunh10-oblique.otf'), 24)
        self.font_medium = pygame.font.Font(
            art('fonts', 'lmromandunh', 'lmromandunh10-regular.otf'), 16)
        self.font_small = pygame.font.Font(
            art('fonts', 'lmromandunh', 'lmromandunh10-oblique.otf'), 14)
        self.clock = pygame.time.Clock()
        self.time_start = time.time()

    def load_audio(self):
        self.audio = Namespace()
        self.audio.start_screen = pygame.mixer.Sound(art('music', 'lemur-start.ogg'))
        self.audio.level_run = pygame.mixer.Sound(art('music', 'lemur-løb.ogg'))
        self.audio.talk = pygame.mixer.Sound(art('music', 'snak.ogg'))

    def load_characters(self):
        self.char = Namespace()
        self.char.lemur = Namespace(
            name='Lemur Lyn',
            surf=pygame.image.load(art('characters', 'lemur.png')))
        self.char.snegl = Namespace(
            name='Snegl Sur',
            surf=pygame.image.load(art('characters', 'snegl.png')))
        self.char.and_ = Namespace(
            name='And Apati',
            surf=pygame.image.load(art('characters', 'and.png')))
        self.char.regnorm = Namespace(
            name='Regnorm Rig',
            surf=pygame.image.load(art('characters', 'regnorm.png')))

    def load_sprite(self):
        self.sprite = Namespace()
        self.sprite.body = pygame.image.load(art('sprite', 'body.png'))
        self.sprite.arm_back = Namespace(
            surf=pygame.image.load(art('sprite', 'arm-back.png')),
            local_pos=(14, 1),
            body_pos=(22, 23))
        self.sprite.arm_front = Namespace(
            surf=pygame.image.load(art('sprite', 'arm-front.png')),
            local_pos=(1, 4),
            body_pos = (25, 25))
        self.sprite.leg_back = Namespace(
            surf=pygame.image.load(art('sprite', 'leg-back.png')),
            local_pos=(15, 0),
            body_pos = (21, 35))
        self.sprite.leg_front = Namespace(
            surf=pygame.image.load(art('sprite', 'leg-front.png')),
            local_pos=(1, 1),
            body_pos = (26, 35))

    def load_objects(self):
        self.objects = Namespace()
        self.objects.ring = pygame.image.load(art('objects', 'ring.png'))

    def show(self):
        pygame.transform.scale(self.screen_base, self.size_scaled,
                               self.screen_scaled)
        pygame.display.flip()

    def step(self):
        self.clock.tick(self.fps)
        self.time = time.time() - self.time_start
        self.frame = self.time * self.fps
        events = pygame.event.get()
        for event in events:
            if event.type == pygame.QUIT:
                raise ExitGame
        return events

    def interval(self, t):
        return (self.time % t) / t

    def render_text(self, font, what, where, color=(255, 255, 255)):
        text = font.render(what, 0, color)
        self.screen_base.blit(text, where)

    def talk_character(self, character, sentences):
        self.audio.talk.play(loops=1)
        i = 0
        while True:
            if i == len(sentences):
                break
            sentence = sentences[i]

            go_on = False
            for event in self.step():
                if event.type == pygame.KEYDOWN \
                   and event.key == pygame.K_SPACE:
                    go_on = True
                    break
            if go_on:
                i += 1
                continue

            red = abs(self.interval(2.7) - 0.5) * 2.0 * 50.0 + 55.0
            self.screen_base.fill((int(red), 0, 0))

            self.render_text(self.font_large, character.name, (200, -5))
            self.screen_base.blit(character.surf, (200, 40))
            self.render_text(self.font_medium, sentence, (20, 140))
            self.render_text(self.font_small, u'Tryk på mellemrum for at fortsætte.',
                             (130, 170), (0, 150, 0))
            self.show()
        self.audio.talk.stop()

    def play_level(self, height_function, rings, conditions):
        self.audio.level_run.play(loops=1)
        x = 0
        x_prev = 0
        score = 0
        rings = rings[:]
        rings.sort()
        x_speed = 0
        is_running = False
        jump_status = -1
        run_speed_last_changed = 0
        turn_base = 0
        t_prev = 0
        jump_h = 0

        while True:
            for event in self.step():
                if event.type == pygame.KEYDOWN:
                    if event.key == pygame.K_RIGHT:
                        is_running = True
                        run_speed_last_changed = self.frame - 1
                    elif event.key == pygame.K_SPACE:
                        jump_status = 0
                        jump_add = 0.5
                        jump_h = 1.0
                        jump_last_changed = self.frame - 1
                elif event.type == pygame.KEYUP:
                    if event.key == pygame.K_RIGHT:
                        is_running = False

            if is_running:
                x_speed += (self.frame - run_speed_last_changed) * 0.01
                run_speed_last_changed = self.frame
                if x_speed > 1.0:
                    x_speed = 1.0
            else:
                x_speed -= (self.frame - run_speed_last_changed) * 0.03
                run_speed_last_changed = self.frame
                if x_speed < 0.0:
                    x_speed = 0

            if jump_status != -1:
                jump_diff = self.frame - jump_last_changed
            if jump_status == 0:
                jump_add *= jump_diff * 1.1
                jump_h += jump_add
                if jump_h >= 100:
                    jump_status = 1
            elif jump_status == 1:
                jump_add /= jump_diff * 1.1
                jump_h -= jump_add
                if jump_add <= 1.0:
                    jump_status = -1
                    jump_h = 0
            if jump_status != -1:
                jump_last_changed = self.frame

            x += int(x_speed * 5.0)
            y = height_function(x) + int(jump_h)

            x_screen = self.width // 2
            y_screen = self.height // 2
            w, h = self.sprite.body.get_size()
            x_sprite = x_screen - w // 2
            y_sprite = y_screen - h // 2
            y_sprite_foot = y_sprite + h

            self.screen_base.fill((0, 0, 255))
            y_diff = (self.height - y_sprite_foot) - height_function(x)
            for i, x_show in zip(range(self.width),
                                 range(x - x_screen, x + x_screen)):
                h = height_function(x_show) + y_diff - int(jump_h)
                if h <= 0:
                    continue
                s = pygame.surface.Surface((1, h))
                s.fill((255, 55, 155))
                self.screen_base.blit(s, (i, self.height - h))

            for xr in rings:
                if x_prev <= xr <= x:
                    score += 1
                    rings.remove(xr)
            x_prev = x

            for xr in rings:
                if xr >= x and xr < x + self.width:
                    xr1 = xr - x - self.objects.ring.get_size()[0] / 2 + x_screen
                    self.screen_base.blit(self.objects.ring,
                                          (xr1, self.height - (height_function(xr) + y_diff - int(jump_h)) - self.objects.ring.get_size()[1]))

            speediness = 1.0 - x_speed + 0.3
            t_new = self.interval(speediness) * 4.0
            if x_speed == 0:
                pass
            elif t_prev <= 1.0 <= t_new:
                turn_base += 1
            elif t_prev <= 2.0 <= t_new:
                turn_base += 1
            elif t_prev <= 3.0 <= t_new:
                turn_base += 1
            elif 3.0 <= t_prev <= 4.0 and 0.0 <= t_new <= 1.0:
                turn_base += 1
            t_prev = t_new

            def draw_subsprite(ns, turn):
                turn %= 4
                angle = turn * -90
                w, h = ns.surf.get_size()
                surf = pygame.transform.rotate(ns.surf, angle)
                x_offset, y_offset = ns.body_pos
                x_local, y_local = ns.local_pos
                if turn == 0:
                    x_offset -= x_local
                    y_offset -= y_local
                elif turn == 1:
                    x_offset -= h - y_local
                    y_offset -= x_local
                elif turn == 2:
                    x_offset -= w - x_local
                    y_offset -= h - y_local
                elif turn == 3:
                    x_offset -= y_local
                    y_offset -= w - x_local
                self.screen_base.blit(surf,
                                      (x_sprite + x_offset, y_sprite + y_offset))

            draw_subsprite(self.sprite.leg_back, (turn_base + 1) % 2 - 1)
            draw_subsprite(self.sprite.arm_back, turn_base + 2)
            self.screen_base.blit(self.sprite.body, (x_sprite, y_sprite))
            draw_subsprite(self.sprite.leg_front, turn_base % 2)
            draw_subsprite(self.sprite.arm_front, turn_base + 2)

            self.render_text(self.font_small, 'Point: {}'.format(score), (5, 0))

            self.show()

            state = Namespace()
            state.score = score
            finish = False
            for c in conditions:
                try:
                    c(state)
                except FinishLevel:
                    finish = True
            if finish:
                break
        self.audio.level_run.stop()

    def start_screen(self):
        background = pygame.image.load(art('start-screen', 'background.png'))
        pupils = pygame.image.load(art('start-screen', 'pupils.png'))
        self.audio.start_screen.play(loops=1)

        while True:
            go_on = False
            for event in self.step():
                if event.type == pygame.KEYDOWN \
                   and event.key in [pygame.K_SPACE, pygame.K_RETURN,
                                     pygame.K_KP_ENTER]:
                    go_on = True
                    break
            if go_on:
                break
            self.screen_base.blit(background, (0, 0))
            scale = abs(self.interval(2.5) - 0.5) * 2.0
            x_pupil = scale * 13 - 12
            y_pupil = scale * 4 - 3
            self.screen_base.blit(pupils, (x_pupil, y_pupil))
            rect = pygame.surface.Surface((206, 30)).convert_alpha()
            alph = abs(self.interval(2.5) - 0.5) * 100.0 + 120.0
            rect.fill((255, 255, 255, int(alph)))
            self.screen_base.blit(rect, (130, 130))
            self.render_text(self.font_small, u'Tryk på mellemrum for at starte.',
                             (130, 130), (0, 0, 0))

            self.show()

        self.audio.start_screen.stop()

    def level1(self):
        self.talk_character(
            self.char.snegl,
            [
                u'Iiih, I har alt for meget ramasjang og tjuhej!',
                u'Jeg har derfor sat fartgrænsen ned til 1 km/t over hele Fartby.',
                u'Ingen kan stoppe mig!'
            ])

        self.talk_character(
            self.char.and_,
            [
                u'Åh nej.  Sikke noget.',
                u'Ingen kan redde os.  Det er også ligemeget.'
            ])

        self.talk_character(
            self.char.lemur,
            [
                u'Jo, jeg kan!',
                u'Jeg kan løbe fremad på højretasten og hoppe på mellemrumstasten.',
                u'Det skal gå STÆRKT!'
            ])

        height_function = lambda x: int(math.sin(x / 20.0) * 20.0)
        rings = [30, 100, 110, 120]
        for i in range(100):
            rings.append(500 + i * 20)
        def end_condition(state):
            if state.score == 104:
                raise FinishLevel
        self.play_level(height_function, rings, [end_condition])

    def level2(self):
        self.talk_character(
            self.char.regnorm,
            [
                u'Godt klaret, Lemur!',
                u'Nu er der atter fart over feltet i Fartby.',
                u'Her er 100 millioner for din hjælp.'
            ])

def parse_args(args):
    arg_parser = argparse.ArgumentParser(description='Spil Lemur Lyn.')
    arg_parser.add_argument('--scale', type=int, metavar='N', default=2,
                            help='apply integral scaling to the source')
    args = arg_parser.parse_args(args)
    return args

def main(args):
    args = parse_args(args)
    game = Game(scale=args.scale, fps=60)
    return game.run()

if __name__ == '__main__':
    sys.exit(main(sys.argv[1:]))
