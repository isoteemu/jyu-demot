#!/usr/bin/env python3
# -*- coding: utf-8 -*-

r""" Demo 4 tehtävä 7 """

import cv2
import numpy


def face_detect(kuva, cascade):
    faceCascade = cv2.CascadeClassifier(cascade)

    # Read the image
    if type(kuva) is numpy.ndarray:
        image = kuva
    else:
        image = cv2.imread(kuva)

    gray = cv2.cvtColor(image, cv2.COLOR_BGR2GRAY)

    # Detect faces in the image
    faces = faceCascade.detectMultiScale(gray,
                                         scaleFactor=1.1,
                                         minNeighbors=5,
                                         minSize=(30, 30),
                                         flags=cv2.CASCADE_SCALE_IMAGE)

    if len(faces) == 0:
        return []

    return faces.tolist()


def etsi_ja_piirra(kuva, cascade):
    kopio = kuva.copy()

    rect = face_detect(kopio, cascade)

    for x1, y1, w, h in rect:
        print("Kasvot:", x1, y1, w, h)
        cv2.rectangle(kopio, (x1, y1), (x1+w, y1+h), (255, 0, 0), 2)

    return kopio

if __name__ == '__main__':

    import argparse

    cmdline = argparse.ArgumentParser(description='TIEA345 Demo 4.7')
    cmdline.add_argument('--cascade', nargs='?', default="/usr/share/OpenCV/haarcascades/haarcascade_frontalface_alt2.xml")
    cmdline.add_argument('--image', nargs='?', default="4-6_wwf.jpg")
    cmdline.add_argument('--dest', nargs='?', default="4-7_kasvot.jpg")

    args = cmdline.parse_args()

    kuva = cv2.imread(args.image)
    ulostus = etsi_ja_piirra(kuva, args.cascade)

    cv2.imwrite(args.dest, ulostus)
