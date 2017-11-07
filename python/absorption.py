import sys

def main(file):
    print("called with file \'%s\'" % file)

if __name__ == '__main__':
    if len(sys.argv) != 2:
        print("usage: %s [file]" % sys.argv[0])
        exit(1)
    main(sys.argv[1])
