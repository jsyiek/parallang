import re

from typing import List


"""
Parses an input file 
"""

class ParseInput:
    def __init__(self, datalines):
        self.datalines = [d.lstrip() for d in datalines]
        self.n = 0

    def __bool__(self):
        return self.n < len(self.datalines)

    def next(self):
        next_line = self.datalines[self.n]
        self.n += 1
        return next_line

    def back(self):
        self.n -= 1

    def peek(self):
        return self.datalines[self.n]


def parse_execution_context(p):
    execution_context_data = {}
    while p and not p.peek().startswith("PE"):
        l = p.next()
        if l.startswith("ExecutionContext"):
            continue
        elif l.startswith("SampleBasedMemoryListener"):
            m = re.search(r"SampleBasedMemoryListener: Samples \(ts, bytes\) List\(((?:\(\d+,\d+\)(?:, ?)?)*)\)", l)
            samples = tuple((int(i.split(",")[0][1:]), int(i.split(",")[1][:-1])) for i in m[1].split(", "))
            execution_context_data["SampleBasedMemoryListener"] = {"samples": samples}
        elif l.startswith("PeakMemoryListener"):
            m = re.search(r"PeakMemoryListener: Peak (\d+)B @ (\d+)", l)
            peak_memory = int(m[1])
            peaked_at = int(m[2])
            execution_context_data["PeakMemoryListener"] = {
                "peak_memory": peak_memory,
                "peaked_at": peaked_at
            }

    return execution_context_data


def parse_time_tally_listener(p):
    time_tally_data = {}
    while p and not p.peek().startswith("ExecutionContext"):
        l = p.next()
        if l.startswith("TimeTallyListener"):
            m = re.search(r"TimeTallyListener: \(total (\d)+\)", l)
            time_tally_data["avg"] = int(m[1])
        else:
            m = re.search(r"([a-zA-Z]+): avg: (\d+), Samples\(((?:-?\d+(?:, )?)*)\)", l)
            event = m[1]
            avg = m[2]
            samples = tuple(int(i) for i in m[3].split(", "))
            time_tally_data[event] = {
                "avg": avg,
                "samples": samples
            }
    return time_tally_data


def parse_latency(p):
    latency_data = {}
    while p and not p.peek().startswith("ExecutionContext"):
        l = p.next()
        if l.startswith("Latency"):
            m = re.search(r"Latency: Avg: (\d+), Samples\(((?:\d+(?:, )?)*)\)", l)
            latency = int(m[1])
            samples = tuple(int(i) for i in m[2].split(", "))
            latency_data["avg"] = latency
            latency_data["samples"] = samples
        elif l.startswith("TimeTallyListener"):
            p.back()
            latency_data["TimeTallyListener"] = parse_time_tally_listener(p)
    return latency_data
              

def parse_pe(d: str):
    p = ParseInput(d.split("\n"))
    
    pe_data = {}
    try:
        while p:
            l = p.next().lstrip()
            if l.startswith("="):
                continue
            elif l.startswith("PE"):
                m = re.search(r"PE \(([0-9]+),([0-9]+)\):", l)
                indices = (int(m[1]), int(m[2]))
                pe_data[indices] = {
                    "Latency": parse_latency(p),
                    "ExecutionContext": parse_execution_context(p)
                }
    except Exception as e:
        p.back()
        print("[DEBUG] problematic line")
        print("[DEBUG]", p.peek())
        raise e
    return pe_data
