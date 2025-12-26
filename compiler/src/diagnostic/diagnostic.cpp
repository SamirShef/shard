#include "../include/diagnostic/diagnostic.h"
#include <iostream>

void Diagnostic::add_part(DiagPart part) {
    errs.push_back(part);
}

bool Diagnostic::has_errs() const {
    return errs.size() != 0;
}

void Diagnostic::print_errs() const {
    for (auto part : errs) {
        std::cerr << part.to_str() << '\n';
    }
}