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

void diag_part_create(Diagnostic &diag, DiagPart part, std::string err_msg);
void diag_part_create(Diagnostic &diag, u16 code, Position pos, DiagLevel level, std::string err_msg);