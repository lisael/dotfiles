from thefuck.shells import shell


def match(command):
    return (command.script.startswith('cat '))


def get_new_command(command):
    return "pygmentize {} ".format(" ".join(command.script_parts[1:]))
