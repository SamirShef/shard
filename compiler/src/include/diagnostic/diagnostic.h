#pragma once
#include "diag_part.h"
#include <vector>

class Diagnostic {
    std::vector<DiagPart> errs;

public:
    Diagnostic() : errs() {}

    void add_part(DiagPart part);
    bool has_errs() const;
    void print_errs() const;
};