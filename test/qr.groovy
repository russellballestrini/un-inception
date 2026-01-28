#!/usr/bin/env groovy
// Use qrencode CLI via ProcessBuilder (libqrencode wrapper)
def proc = ["qrencode", "-t", "UTF8", "-m", "0", "unsandbox-qr-ok"].execute()
def output = proc.text
proc.waitFor()
def rows = output.split('\n').findAll { it.trim() }.size()
println "QR:unsandbox-qr-ok:ROWS:${rows}"
