from cheat.colorize import Colorize
from cheat.utils import Utils
import io
import os


def is_empty_line(line):
    return len(line.strip()) == 0

def is_definiton_line(line):
    for c in line:
        if c != "#" and c != " ":
            return True

def previous_line_is_empty(index, content):
    """ check if the previous index is empty """
    if index == 0:
        return True
    prev_line = content[index - 1]
    return is_empty_line(prev_line)

def next_line_is_empty(index, content):
    """ check if the next index is empty """
    if index == len(content) - 1:
        return True
    next_line = content[index + 1]
    return is_empty_line(next_line)

def get_start_index(index, content):
    while not previous_line_is_empty(index, content):
        index = index - 1
    return index

def get_end_index(index, content):
    while not next_line_is_empty(index, content):
        index = index + 1
    return index


class Sheets:

    def __init__(self, config):
        self._config = config
        self._colorize = Colorize(config)

        # Assembles a dictionary of cheatsheets as name => file-path
        self._sheets = {}
        sheet_paths = [
            config.cheat_user_dir
        ]

        # merge the CHEAT_PATH paths into the sheet_paths
        if config.cheat_path:
            for path in config.cheat_path.split(os.pathsep):
                if os.path.isdir(path):
                    sheet_paths.append(path)

        if not sheet_paths:
            Utils.die('The CHEAT_USER_DIR dir does not exist '
                      + 'or the CHEAT_PATH is not set.')

        # otherwise, scan the filesystem
        for cheat_dir in reversed(sheet_paths):
            self._sheets.update(
                dict([
                    (cheat, os.path.join(cheat_dir, cheat))
                    for cheat in os.listdir(cheat_dir)
                    if not cheat.startswith('.')
                    and not cheat.startswith('__')
                ])
            )

    def directories(self):
        """ Assembles a list of directories containing cheatsheets """
        sheet_paths = [
            self._config.cheat_user_dir,
        ]

        # merge the CHEATPATH paths into the sheet_paths
        for path in self._config.cheat_path.split(os.pathsep):
            sheet_paths.append(path)

        return sheet_paths

    def get(self):
        """ Returns a dictionary of cheatsheets as name => file-path """
        return self._sheets

    def list(self):
        """ Lists the available cheatsheets """
        sheet_list = ''
        pad_length = max([len(x) for x in self.get().keys()]) + 4
        for sheet in sorted(self.get().items()):
            sheet_list += sheet[0].ljust(pad_length) + sheet[1] + "\n"
        return sheet_list

    def search(self, term):
        """ Searches all cheatsheets for the specified term """
        result = ''

        for cheatsheet in sorted(self.get().items()):
            match = ''
            # for line in io.open(cheatsheet[1], encoding='utf-8'):
            #     if term in line:
            #         match += '  ' + self._colorize.search(term, line)
            io_wrapper = io.open(cheatsheet[1], encoding='utf-8')
            content = io_wrapper.readlines()
            taken_index = []
            for index, line in enumerate(content):
                if term in line and index not in taken_index:
                    start = get_start_index(index, content)
                    stop = get_end_index(index, content)
                    for i in range(start, stop+1):
                        match += '  ' + self._colorize.search(term, content[i])
                        taken_index.append(i)
                    match += '\n'

            if match != '':
                result += cheatsheet[0] + ":\n" + match + "\n"


        return result

