#include <iostream>
#include <utility>

class Mask {
   public:
    Mask(int n_row, int n_col) : n_row_(n_row), n_col_(n_col) {
        data_ = new bool[n_row * n_col];

        for (int i = 0; i < n_row; i++) {
            for (int j = 0; j < n_col; j++) {
                if (j >= n_col - i) {
                    data_[i + j * n_row_] = false;
                } else {
                    data_[i + j * n_row_] = true;
                }
            }
        }
    };
    Mask(int n_dev) : Mask(n_dev, n_dev){};
    ~Mask() { delete[] data_; }

    bool* begin() { return data_; }
    int n_row() { return n_row_; }
    int n_col() { return n_col_; }
    int size() { return n_row_ * n_col_; }

    bool& operator()(int i, int j) { return data_[i + j * n_row_]; }

    class RowProxy {
       public:
        RowProxy(Mask& mask, int len, int idx) : len_(len), idx_(idx) {
            data_ = new bool*[len];
            for (int i = 0; i < len; i++) {
                data_[i] = mask.begin() + idx + i * mask.n_row();
            }
        };

        ~RowProxy() { delete[] data_; }

        void operator=(bool val) {
            for (int i = 0; i < len_; i++) {
                *data_[i] = val;
            }
        }
        bool** data() { return data_; }

       private:
        int len_;
        int idx_;
        bool** data_;
    };
    RowProxy row(int idx) { return RowProxy(*this, n_col_, idx); }

    friend void swap(Mask& first, Mask& second) noexcept {
        using std::swap;
        swap(first.data_, second.data_);
        swap(first.n_row_, second.n_row_);
        swap(first.n_col_, second.n_col_);
    }

    Mask& operator=(Mask other) {
        swap(*this, other);
        return *this;
    }

    Mask(Mask& other) : n_row_(other.n_row_), n_col_(other.n_col_) {
        data_ = new bool[other.size()];
        for (int i = 0; i < other.size(); i++) {
            data_[i] = other.data_[i];
        }
    }

    friend std::ostream& operator<<(std::ostream& os, Mask& mask) {
        for (int i = 0; i < mask.n_row(); i++) {
            for (int j = 0; j < mask.n_col(); j++) {
                os << (mask(i, j) ? "T" : "F") << " ";
            }
            os << std::endl;
        }
        return os;
    }

   private:
    int n_row_, n_col_;
    bool* data_;
};