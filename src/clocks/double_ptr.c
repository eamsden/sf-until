double get_double(double * dp, int off) {
	return *(dp + off);
}

void put_double(double * dp, int off, double d) {
	*(dp + off) = d;
}
