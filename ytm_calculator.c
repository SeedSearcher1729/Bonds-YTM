#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <float.h>
typedef struct {
    double periodic;
    double annual_effective;
    double nominal_apr;
} YTMResult;
double bond_price_from_periodic_rate(double r, double face, double annual_coupon_rate, double years, int periods_per_year) {
    int N = (int) round(years*periods_per_year);
    if (N<=0) return 0.0;
    double coupon = face * annual_coupon_rate / periods_per_year;
    double total = 0.0;
    for (int t = 1; t <= N; t++) {
        total += coupon / pow(1 + r, t);
    }
    total += face / pow(1 + r, N);
    return total;
}
YTMResult ytm_bisection(double face, double annual_coupon_rate, double years, double price, int periods_per_year, double tol, int max_iter){
    YTMResult res = {NAN, NAN, NAN};
    int N = (int) round(years * periods_per_year);
    if (N<=0){
        fprintf(stderr, "Error: Number of periods must be positive.\n");
        return res;
    }
    double coupon = face * annual_coupon_rate / periods_per_year;
    if (fabs(coupon) < 1e-12){
        if (price <= 0){
            fprintf(stderr, "Error: price must be positive for zero-coupon bonds.\n");
            return res;
        }
        double periodic = pow(face / price, 1.0 / N) - 1.0;
        res.periodic = periodic;
        res.annual_effective = pow(1 + periodic, periods_per_year) - 1.0;
        res.nominal_apr = periodic * periods_per_year;
        return res;
    }
    double low = -0.999999;
    double high = 10.0;
    double f_low = bond_price_from_periodic_rate(low, face, annual_coupon_rate, years, periods_per_year) - price;
    double f_high = bond_price_from_periodic_rate(high, face, annual_coupon_rate, years, periods_per_year) - price;
    int tries = 0;
    while (f_low * f_high > 0 && tries < 100){
        high *= 2.0;
        f_high = bond_price_from_periodic_rate(high, face, annual_coupon_rate, years, periods_per_year) - price;
        tries++;
    }
    if (f_low * f_high > 0){
        fprintf(stderr, "Unable to bracket root for YTM. Check inputs (price, coupon, face, years).\n");
        return res;
    }
    double mid, f_mid;
    for (int iter = 0; iter < max_iter; ++iter){
        mid = 0.5 * (low + high);
        f_mid = bond_price_from_periodic_rate(mid, face, annual_coupon_rate, years, periods_per_year) - price;
        if (fabs(f_mid) < tol){
            res.periodic = mid;
            res.annual_effective = pow(1 + mid, periods_per_year) - 1.0;
            res.nominal_apr = mid * periods_per_year;
            return res;
        }
        if (f_low * f_mid < 0){
            high = mid;
            f_high = f_mid;
        } else {
            low = mid;
            f_low = f_mid;
        }
    }
    res.periodic = 0.5 * (low + high);
    res.annual_effective = pow(1 + res.periodic, periods_per_year) - 1.0;
    res.nominal_apr = res.periodic * periods_per_year;
    return res;
}
void print_usage(const char *prog){
    printf("Usage: %s [--face-value <num>] [--coupon-rate <percent>] [--years <num>] [--price <num.] [--periods-per-year <int>] [--tolerance <num>] [--max-iterations <int>]\n", prog);
    printf("If no arguements are provdied the program will prompt interactively.\n");
    printf("Example:\n %s --face-value 1000 --coupon-rate 8 --years 10 --price 950 --periods-per-year 2\n", prog);
}
int main(int argc, char **argv){
    double face = NAN, coupon_percent = NAN, years = NAN, price = NAN;
    int periods = 2;
    double tol = 1e-9;
    int max_iter = 200;
 
    if (argc == 1) {
     printf("Yield-to-Maturity (YTM) calculator (C)\nEnter face/par value (e.g., 1000): ");
        if (scanf("%lf", &face) != 1) return 1;
        printf("Enter annual coupon rate in percent (e.g., 8 for 8%%): ");
        if (scanf("%lf", &coupon_percent) != 1) return 1;
        printf("Enter years to maturity (e.g., 10): ");
        if (scanf("%lf", &years) != 1) return 1;
        printf("Enter current market price: ");
        if (scanf("%lf", &price) != 1) return 1;
        printf("Enter periods per year (1=annual, 2=semiannual, 4=quarterly) [default=2]: ");
        if (scanf("%d", &periods) != 1) periods = 2;
    } else {
        for (int i = 1; i < argc; ++i) {
            if (strcmp(argv[i], "--face-value") == 0 || strcmp(argv[i], "-f") == 0) {
                if (i + 1 < argc) face = atof(argv[++i]);
            } else if (strcmp(argv[i], "--coupon-rate") == 0 || strcmp(argv[i], "-c") == 0) {
                if (i + 1 < argc) coupon_percent = atof(argv[++i]);
            } else if (strcmp(argv[i], "--years") == 0 || strcmp(argv[i], "-y") == 0) {
                if (i + 1 < argc) years = atof(argv[++i]);
            } else if (strcmp(argv[i], "--price") == 0 || strcmp(argv[i], "-p") == 0) {
                if (i + 1 < argc) price = atof(argv[++i]);
            } else if (strcmp(argv[i], "--periods-per-year") == 0 || strcmp(argv[i], "-m") == 0) {
                if (i + 1 < argc) periods = atoi(argv[++i]);
            } else if (strcmp(argv[i], "--tolerance") == 0) {
                if (i + 1 < argc) tol = atof(argv[++i]);
            } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
                print_usage(argv[0]);
                return 0;
            } else {
                fprintf(stderr, "Unknown option: %s\n", argv[i]);
                print_usage(argv[0]);
                return 1;
            }
        }
    }
    if (isnan(face) || isnan(coupon_percent) || isnan(years) || isnan(price)) {
        fprintf(stderr, "Missing required inputs. Use --help for usage or run without args for interactive mode.\n");
        return 1;
    }
    if (periods <= 0) {
        fprintf(stderr, "periods-per-year must be positive integer\n");
        return 1;
    }
    double coupon_rate = coupon_percent / 100.0;
    YTMResult r = ytm_bisection(face, coupon_rate, years, price, periods, tol, max_iter);
    if (isnan(r.periodic)) {
        fprintf(stderr, "Failed to compute YTM.\n");
        return 1;
    }
    printf("Inputs: face=%.2f, coupon=%.6f%%, years=%.6f, price=%.2f, periods/year=%d\n",
           face, coupon_rate * 100.0, years, price, periods);
    printf("Periodic YTM (per period): %.9f%%\n", r.periodic * 100.0);
    printf("Annualized effective YTM: %.9f%%\n", r.annual_effective * 100.0);
    printf("Nominal APR (periodic * m): %.9f%%\n", r.nominal_apr * 100.0);
    return 0;
    }
