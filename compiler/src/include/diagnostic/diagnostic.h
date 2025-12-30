#pragma once
#include "diag_part.h"
#include <vector>

class Diagnostic {
    std::vector<DiagPart> errs;

public:
    explicit Diagnostic() : errs() {}

    void add_part(DiagPart part);
    bool has_errs() const;
    void print_errs() const;
    void clear();
};

void diag_part_create(Diagnostic &diag, DiagPart part, std::string &src, u64 space_len, u64 highlighter_len, std::string err_msg);

void diag_part_create(Diagnostic &diag, DiagPart part, std::string &src, std::string highlighter);