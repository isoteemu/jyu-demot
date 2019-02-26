#!/usr/bin/env python

"""
Demo 4 Tehtävä 6
Pretty much kopioitu suoraan OpenCVn tutoriaalista
"""

import cv2
from matplotlib import pyplot as plt  

kuva_wwf = cv2.imread("4-6_wwf.jpg")
kuva_wcw = cv2.imread("4-6_wcw.jpg")

orb = cv2.ORB_create()

keypoints_wwf, descriptions_wwf = orb.detectAndCompute(kuva_wwf, None)
keypoints_wcw, descriptions_wcw = orb.detectAndCompute(kuva_wcw, None)

bf = cv2.BFMatcher()
matches = bf.knnMatch(descriptions_wwf, descriptions_wcw, k=2)

osumat = []
for m,n in matches:
    if m.distance < 0.85*n.distance:
        osumat.append([m])

kuva_orb = cv2.drawMatchesKnn(kuva_wwf, keypoints_wwf,
                              kuva_wcw, keypoints_wcw,
                              osumat, None, flags=2)

plt.imshow(cv2.cvtColor(kuva_orb, cv2.COLOR_BGR2RGB))
plt.savefig('4-6_orb.png')
