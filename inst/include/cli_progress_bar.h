#include <R_ext/Print.h>
#include <RcppArmadillo.h>

#include <progress_bar.hpp>

#if !defined(WIN32) && !defined(__WIN32) && !defined(__WIN32__)
    #include <Rinterface.h>
#endif

class CliProgressBar : public ProgressBar {
   public:
    CliProgressBar() { reset(); }

    ~CliProgressBar() {}

   public:
    void display() { REprintf("\033[37mRunning simulations "); }

    void update(float progress) {
        _update_ticks_display(progress);
        if (_ticks_displayed >= _max_ticks) _finalize_display();
    }

    void end_display() {
        update(1);
        reset();
    }

    void reset() {
        Rcpp::Environment cli = Rcpp::Environment::namespace_env("cli");
        Rcpp::Function console_width = cli["console_width"];

        // From fiddling around with it, it seems that dividing the console
        // width by 2 produces the best display.
        _max_ticks = std::floor(Rcpp::as<int>(console_width()) / 2);
        _ticks_displayed = 0;
        _finalized = false;
    }

   protected:
    void _update_ticks_display(float progress) {
        int nb_ticks = _compute_nb_ticks(progress);
        int delta = nb_ticks - _ticks_displayed;
        if (delta > 0) {
            REprintf("\r");
            _ticks_displayed = nb_ticks;
            _display_ticks(progress);
        }
    }

    void _finalize_display() {
        if (_finalized) return;

        REprintf("\n");
        flush_console();
        _finalized = true;
    }

    int _compute_nb_ticks(float progress) { return int(progress * _max_ticks); }

    void _display_ticks(double progress) {
        REprintf("\033[37mRunning simulations ");
        for (int i = 0; i < _ticks_displayed; ++i) {
            REprintf("\033[32m\u25A0");
            R_FlushConsole();
        }
        for (int i = 0; i < (_max_ticks - _ticks_displayed); i++) {
            REprintf(" ");
            R_FlushConsole();
        }
        char end[30];
        sprintf(end, "\033[37m | %d%%", int(progress * 100));
        REprintf("%s", end);
    }

    // N.B: does nothing on windows
    void flush_console() {
#if !defined(WIN32) && !defined(__WIN32) && !defined(__WIN32__)
        R_FlushConsole();
#endif
    }

   private:
    int _max_ticks;        // the total number of ticks to print
    int _ticks_displayed;  // the nb of ticks already displayed
    bool _finalized;
};