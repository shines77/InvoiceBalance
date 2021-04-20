#pragma once

#include <stdint.h>
#include <stddef.h>

#include <cstdint>
#include <cstddef>
#include <iostream>
#include <fstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <cstring>
#include <memory>
#include <exception>
#include <stdexcept>

class IniFile
{
public:
    typedef std::size_t         size_type;

private:
    std::string                 filename_;
    std::vector<std::string>    lines_;
    std::unordered_map<std::string, std::string> map_;

    static std::string          empty_string;

public:
    IniFile() {
    }
    IniFile(const char * filename) : filename_(filename) {
        this->open(this->filename_.c_str());
    }

    virtual ~IniFile() {}

    const std::vector<std::string> & get_lines() const {
        return this->lines_;
    }

    void clear() {
        this->lines_.clear();
    }

    static size_type skip_whitespace_chars(const std::string & str, size_type start = 0) {
        size_type max_len = str.size();
        size_type pos = start;
        while (pos < max_len) {
            char ch = str[pos];
            if (ch == ' ' || ch == '\t' || ch == '\b' || ch == '\v')
                pos++;
            else
                break;
        }
        return ((pos < max_len) ? pos : std::string::npos);
    }

    static size_type find_char(const std::string & str, size_type start = 0, char delim = ' ') {
        size_type max_len = str.size();
        size_type pos = start;
        while (pos < max_len) {
            char ch = str[pos];
            if (ch != delim)
                pos++;
            else
                break;
        }
        return ((pos < max_len) ? pos : std::string::npos);
    }

    static size_type copy_string(const std::string & src, std::string & dest,
                                 size_type first = 0, size_type last = size_type(-1)) {
        size_type max_len = src.size();
        if (last == size_type(-1) || last > max_len) {
            last = max_len;
        }
        if (last > first) {
            dest.clear();
            size_type pos = first;
            while (pos < last) {
                dest.push_back(src[pos]);
                pos++;
            }
        }
        else {
            dest = "";
        }
        return dest.size();
    }

    int open(const char * filename) {
        int result = -1;
        if (filename != nullptr) {
            this->filename_ = filename;
            result = this->open();
        }
        return result;
    }

    int open() {
        int result = 0;
        if (this->filename_.empty() || this->filename_ == "") {
            return -1;
        }
        const char * filename = this->filename_.c_str();
        if (filename == nullptr) {
            return -1;
        }
        this->clear();

        std::ifstream ifs;
        try {
            ifs.open(filename);
            if (ifs.good()) {
                while (!ifs.eof()) {
                    static const size_t kLineBufSize = 256;
                    char line[kLineBufSize];
                    std::memset(line, 0, kLineBufSize);
                    ifs.getline(line, kLineBufSize);
                    std::string strLine = line;
                    if (strLine.size() > 0) {
                        this->lines_.push_back(std::move(strLine));
                    }
                }                
            }
            ifs.close();
        }
        catch (std::exception & ex) {
            std::cout << "Exception: " << ex.what() << std::endl << std::endl;
            result = -2;
        }
        return result;
    }

    int parse() {
        int count = 0;
        size_type line_size = this->lines_.size();
        for (size_t i = 0; i < line_size; i++) {
            const std::string & line = this->lines_[i];
            std::string key, value;
            size_type start, end;
            size_type pos = skip_whitespace_chars(line, 0);
            if (pos != std::string::npos) {
                start = pos;
                char ch = line[pos];
                if (ch == '[') {
                    // Section
                    pos = find_char(line, pos + 1, ']');
                    if (pos != std::string::npos) {
                        end = pos;
                        count++;
                    }
                }
                else if (ch == '#' || ch == '\'' || ch == ';' || ch == '@') {
                    // Comment, skip it.
                }
                else {
                    // Other: Key-Value
                    pos = find_char(line, pos, '=');
                    if (pos != std::string::npos) {
                        end = pos;
                        if (start < end) {
                            copy_string(line, key, start, end);
                            pos++;
                            copy_string(line, value, pos, line.size());
                            if (!this->map_.count(key)) {
                                this->map_.insert(std::make_pair(key, value));
                            }
                            else {
                                this->map_[key] = value;
                            }
                            count++;
                        }
                    }
                }
            }
        }
        return count;
    }

    bool contains(const std::string & key) const {
        return (this->map_.count(key) > 0);
    }

    const std::string & values(const std::string & key) const {
        auto const & iter = this->map_.find(key);
        if (iter != this->map_.end()) {
            return iter->second;
        }
        else {
            return IniFile::empty_string;
        }
    }
};

#if defined(_MSC_VER)
__declspec(selectany) std::string IniFile::empty_string;
#else
std::string IniFile::empty_string;
#endif
