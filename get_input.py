#! /usr/bin/env python3

import sqlite3
import shutil
import os
import argparse
import atexit
import http.client
import tempfile

HOME = os.environ["HOME"]
COOKIES=f"{HOME}/Library/Application Support/Firefox/Profiles/" \
    + "su1hbauz.default-release/cookies.sqlite"
TEMP = tempfile.mktemp()
COMMAND = 'SELECT name, value FROM moz_cookies WHERE host = ".adventofcode.com"'
AOC = "adventofcode.com"

def delete_file():
    # print(f"Deleting {TEMP}")
    os.remove(TEMP)

def main():
    # Startup
    shutil.copy(COOKIES, TEMP)
    atexit.register(delete_file)
    conn = sqlite3.connect(TEMP)
    cursor = conn.execute(COMMAND)
    cookies = cursor.fetchall()
    connection = http.client.HTTPSConnection(AOC, port=443)

    # Send connection
    connection.connect()
    connection.request(
        "GET",
        f"/2024/day/{DAY}/input",
        headers={
            "Accept": "text/html",
            "Cookie": "; ".join(f"{k}={v}" for k, v in cookies),
            "Host": "adventofcode.com"
        }
    )
    response = connection.getresponse()
    assert response.status == 200

    # Shutdown
    with open(OUTFILE, "wb") as f:
        f.write(response.read())


if __name__ == "__main__":
    global DAY, OUTFILE
    parser = argparse.ArgumentParser()
    parser.add_argument("day", type=int)
    parser.add_argument("-o", "--output-file", required=True)
    args = parser.parse_args().__dict__
    DAY = args["day"]
    OUTFILE = args["output_file"]
    assert 1 <= DAY <= 25
    main()
