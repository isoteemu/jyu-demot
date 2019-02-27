#!/usr/bin/env python3
# -*- coding: utf-8 -*-

""" Demo 4 Tehtävä 8 """

from picamera.array import PiRGBArray
from picamera import PiCamera
import cv2
import time

from t7 import etsi_ja_piirra

if __name__ == "__main__":

    import argparse

    cmdline = argparse.ArgumentParser(description='TIEA345 Demo 4.8')
    cmdline.add_argument('--cascade', nargs='?', default="/usr/share/OpenCV/haarcascades/haarcascade_frontalface_alt2.xml")
    cmdline.add_argument('--dest', nargs='?', default="4-8_kasvot.jpg")

    args = cmdline.parse_args()

    camera = PiCamera()
    rawCapture = PiRGBArray(camera)

    time.sleep(0.1)

    camera.capture(rawCapture, format="bgr")
    kuva = rawCapture.array

    ulostus = etsi_ja_piirra(kuva, args.cascade)

    cv2.imwrite(args.dest, ulostus)
