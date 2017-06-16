// g++ -std=c++1z -o find-file find-file.cc
#include <regex>
#include <vector>
#include <unistd.h>
// #include <algorithm>
// #include <iostream>
// #include <iterator>

int main(int argc, char **argv)
{
    const char *emacsclient = "/usr/bin/emacsclient";
    bool create_frame = true;
    bool quiet = true;
    std::vector<const char *> v{ emacsclient };
    std::string arg;

    if (argc == 2) {
        arg = argv[1];

        if (arg == "--kill") {
            create_frame = false;
            arg = "(kill-emacs)";
        } else if (arg == "--daemon") {
            create_frame = false;
            quiet = false;
            v.push_back("-a");
            v.push_back("");
            arg = "nil";
        } else {
            std::regex r("[\"\\\\]");
            arg = "(find-file \"" + std::regex_replace(arg, r, "\\$&") + "\")";
        }

        if (!arg.empty()) {
            v.push_back("-e");
            v.push_back(arg.c_str());
        }
    } else if (argc == 1) {
        v.push_back("-e");
        v.push_back("(buffer-menu)");
    }

    if (quiet) {
        v.insert(v.begin() + 1, "-q");
    }

    if (create_frame) {
        v.insert(v.begin() + 1, "-c");
    }

    v.push_back(nullptr);

    // std::copy(v.begin(), v.end() - 1, std::ostream_iterator<const char *>(std::cout, " "));
    // std::cout << std::endl;

    return execv(emacsclient, (char* const*)v.data());
}
