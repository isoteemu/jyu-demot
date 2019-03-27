#!/usr/bin/env python3

import os
import time
import datetime

import Adafruit_DHT
import gspread
from oauth2client.service_account import ServiceAccountCredentials

import RPi.GPIO as GPIO
from picamera import PiCamera

import http.server
import socketserver

class Demo3:

    TEMP_TYPE = 11
    PIN_TEMP = 21

    GSPREAD_NIMI = "TIEA345 Demo3"
    OAUTH_AVAIN = "TIEA345 Demo3-e3d37f9b352b.json"

    PIN_PIR = 24

    CCTV_DIR = "%s/Pictures" % os.environ['HOME']
    HTTP_PORT = 31337

    def __init__(self):
        GPIO.setmode(GPIO.BCM)
        pass

    def lampotila(self):
        sensoridata = self.sensori_temp()
        print("Lämpötila: %i Kosteus: %i" % sensoridata)


    def excel(self):
        """Tallenna Google speadsheettiin"""
        taulukko = self.google_auth()
        lampotila, kosteus = self.sensori_temp()
        print(taulukko.append_row((u"%s" % datetime.datetime.now(), lampotila, kosteus)))


    def cctv(self):
        """Tarkkaile ympäristöä, ja ota kuva jos liikettä"""
        camera = PiCamera()

        GPIO.setup(self.PIN_PIR, GPIO.IN)

        while(True):
            if GPIO.input(self.PIN_PIR):
                kuva = "%s/%s.jpg" % (self.CCTV_DIR, datetime.datetime.now())
                print("Liikettä havaittu: %s" % kuva)
                camera.capture(kuva)

            time.sleep(1)


    def http(self):
        """Luo http serveri"""
        os.chdir(self.CCTV_DIR)

        Handler = http.server.SimpleHTTPRequestHandler

        httpd = socketserver.TCPServer(("", self.HTTP_PORT), Handler)
        sa = httpd.socket.getsockname()
        print("HTTP serveri kuuntelee portissa: {0[0]}:{0[1]}".format(sa))
        httpd.serve_forever()


    def sensori_temp(self):
        return Adafruit_DHT.read_retry(self.TEMP_TYPE, self.PIN_TEMP)


    def google_auth(self):
        """Connect to Google Docs spreadsheet and return the first worksheet."""
        scope = ['https://spreadsheets.google.com/feeds',
                 'https://www.googleapis.com/auth/drive']
        credentials = ServiceAccountCredentials.from_json_keyfile_name(self.OAUTH_AVAIN, scope)
        gc = gspread.authorize(credentials)
        worksheet = gc.open(self.GSPREAD_NIMI).sheet1
        return worksheet


if __name__ == "__main__":
    import argparse

    toiminnot = ["lampotila","excel","cctv","http"]

    cmdline = argparse.ArgumentParser(description="TIEA345 Demo 3")
    cmdline.add_argument("toiminto", help="Suoritettava toiminto.",
                         choices=toiminnot)

    args = cmdline.parse_args()

    if args.toiminto in toiminnot:
        demo = Demo3()
        getattr(demo, args.toiminto)()

    else:
        cmdline.error("Ei toimintoa")
