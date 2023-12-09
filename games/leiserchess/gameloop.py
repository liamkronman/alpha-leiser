#!/usr/bin/env python3
import sys
import subprocess

class Engine:
    def __init__(self):
        self.path = "./leiserchess"
        self.proc = subprocess.Popen(self.path, stdout=subprocess.PIPE, stdin=subprocess.PIPE, bufsize=1, universal_newlines=True)

    def send_command(self, cmd):
        print(f"Sending command: {cmd}")
        self.proc.stdin.write(cmd + "\n")
        self.proc.stdin.flush()

    def read_response(self):
        response = ""
        while True:
            line = self.proc.stdout.readline()
            if line.strip() == "DoneDisplay":
                break
            response += line
        return response

    def display(self):
        self.send_command("display")
        return self.read_response()

    def kill(self):
        self.proc.terminate()

def main():
    engine = Engine()
    while True:
        command = input("Enter command: ")
        if command == "quit":
            break
        elif command == "display":
            print(engine.display())
        else:
            engine.send_command(command)

if __name__ == "__main__":
    main()