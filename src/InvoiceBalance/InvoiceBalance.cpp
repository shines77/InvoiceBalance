
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <time.h>
#include <assert.h>

#include <limits>

#include "CountOf.h"
#include "IniFile.h"

static const double kDefaultTotalPrice = 120000.0;
static const double kDefaultFluctuation = 2.0;

static const size_t kMaxPriceCount = 10;

struct AmountRange {
    int min;
    int max;

    AmountRange() : min(0), max(0) {}
    AmountRange(int min, int max) : min(min), max(max) {}
    AmountRange(const AmountRange & src) : min(src.min), max(src.max) {}

    AmountRange & operator = (const AmountRange & rhs) {
        if (&rhs != this) {
            this->min = rhs.min;
            this->max = rhs.max;
        }
        return *this;
    }
};

static double default_goods_prices[] = {
    212.00,
    172.5,
    226.0
};

static AmountRange default_goods_amount_ranges[] = {
    { 100, 0 },
    { 100, 0 },
    { 100, 0 }
};

static double s_total_price = kDefaultTotalPrice;
static double s_fluctuation = kDefaultFluctuation;

static double *      s_goods_prices = nullptr;
static AmountRange * s_goods_amount_ranges = nullptr;

static size_t        s_goods_price_count = 0;

struct RoundingType {
    enum {
        RoundDown,
        RoundUp,
        HalfAdjust
    };
};

uint32_t next_random32()
{
#if (RAND_MAX == 0x7FFF)
    return (((rand() & 0x7FFF) << 30) | ((rand() & 0x7FFF) << 15) | (rand() & 0x7FFF));
#else
    return rand();
#endif
}

//
// See: https://www.zhihu.com/question/29971598
//
double next_random_box_muller(double mu, double sigma)
{
    const double epsilon = std::numeric_limits<double>::min();
    const double two_pi = 2.0 * 3.14159265358979323846;

    static double z0, z1;
    static bool generate;
    generate = !generate;

    if (!generate) {
        return (z1 * sigma + mu);
    }

    double u1, u2;
    do {
        u1 = rand() * (1.0 / RAND_MAX);
        u2 = rand() * (1.0 / RAND_MAX);
    } while (u1 <= epsilon);

    z0 = sqrt(-2.0 * log(u1)) * cos(two_pi * u2);
    z1 = sqrt(-2.0 * log(u1)) * sin(two_pi * u2);
    return (z0 * sigma + mu);
}

int32_t next_random_i32(int32_t min_num, int32_t max_num)
{
    if (min_num < max_num)
        return (min_num + (next_random32() % uint32_t(max_num - min_num + 1)));
    else if (min_num > max_num)
        return (max_num + (next_random32() % uint32_t(min_num - max_num + 1)));
    else
        return min_num;
}

int64_t next_random_i64(int64_t min_num, int64_t max_num)
{
    if (min_num < max_num)
        return (min_num + (next_random32() % uint32_t(max_num - min_num + 1)));
    else if (min_num > max_num)
        return (max_num + (next_random32() % uint32_t(min_num - max_num + 1)));
    else
        return min_num;
}

double normal_dist_next_random()
{
    const double l_limit = -3.0, r_limit = 3.0;
    double randomf = next_random_box_muller(0.0, 1.0);
    if (randomf > r_limit)
        randomf = r_limit;
    else if (randomf < l_limit)
        randomf = l_limit;

    // normalize to [-1, 1]
    randomf /= r_limit;

    // translation to [0, 1]
    randomf = (randomf + 1.0) / 2.0;
    return randomf;
}

int32_t normal_dist_random_i32(int32_t min_num, int32_t max_num)
{
    double randomf = normal_dist_next_random();
    assert(randomf >= 0.0 && randomf <= 1.0);

    if (min_num < max_num)
        return (min_num + int32_t(randomf * (max_num - min_num)));
    else if (min_num > max_num)
        return (max_num + int32_t(randomf * (min_num - max_num)));
    else
        return min_num;
}

int64_t normal_dist_random_i64(int64_t min_num, int64_t max_num)
{
    double randomf = normal_dist_next_random();

    if (min_num < max_num)
        return (min_num + int64_t(randomf * (max_num - min_num)));
    else if (min_num > max_num)
        return (max_num + int64_t(randomf * (min_num - max_num)));
    else
        return min_num;
}

double normal_dist_random_f(double minimun, double maximum)
{
    double randomf = normal_dist_next_random();

    if (minimun < maximum)
        return (minimun + randomf * (maximum - minimun));
    else if (minimun > maximum)
        return (maximum + randomf * (minimun - maximum));
    else
        return minimun;
}

double round_price(double price, double precision = 100.0, int round_type = RoundingType::HalfAdjust)
{
    if (round_type == RoundingType::RoundDown)
        return floor(price * precision) / precision;
    else if (round_type == RoundingType::RoundUp)
        return ceil(price * precision) / precision;
    else
        return floor(price * precision + 0.5) / precision;
}

struct GoodsInvoice
{
    bool            auto_release;
    size_t          count;
    double *        prices;
    AmountRange *   amount_ranges;
    size_t *        amounts;

    GoodsInvoice() : auto_release(false), count(0),
                     prices(nullptr), amount_ranges(nullptr), amounts(nullptr) {
    }

    GoodsInvoice(size_t goods_count, double goods_prices[],
                 AmountRange goods_amount_ranges[] = nullptr,
                 size_t goods_amounts[] = nullptr)
        : auto_release(false), count(goods_count),
          prices(goods_prices), amount_ranges(goods_amount_ranges), amounts(goods_amounts) {
    }

    GoodsInvoice(const GoodsInvoice & other)
        : auto_release(false), count(0),
          prices(nullptr), amount_ranges(nullptr), amounts(nullptr) {
        this->construct_copy(other);
    }

    ~GoodsInvoice() {
        this->destroy();
    }

    size_t size() const {
        return this->count;
    }

    double moneys(size_t index) const {
        assert(index < this->count);
        return round_price(this->prices[index] * this->amounts[index]);
    }

    void destroy() {
        if (this->auto_release) {
            if (this->prices) {
                delete[] this->prices;
                this->prices = nullptr;
            }
            if (this->amount_ranges) {
                delete[] this->amount_ranges;
                this->amount_ranges = nullptr;
            }
            if (this->amounts) {
                delete[] this->amounts;
                this->amounts = nullptr;
            }
        }
    }

    GoodsInvoice & operator = (const GoodsInvoice & rhs) {
        this->copy(rhs);
        return *this;
    }

    void attach(const GoodsInvoice & other) {
        if (&other != this) {
            this->destroy();

            this->auto_release  = false;
            this->count         = other.count;
            this->prices        = other.prices;
            this->amount_ranges = other.amount_ranges;
            this->amounts       = other.amounts;
        }
    }

    bool create_new_invoice(const GoodsInvoice & other) {
        assert(other.count != 0);
        size_t goods_count = other.count;
        this->auto_release = true;
        this->count = goods_count;

        // price
        double * new_goods_price = new double[goods_count];
        if (new_goods_price == nullptr) {
            return false;
        }

        if (other.prices != nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_price[i] = other.prices[i];
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_price[i] = 0.0;
            }
        }
        this->prices = new_goods_price;

        // amount range
        AmountRange * new_goods_amount_ranges = new AmountRange[goods_count];
        if (new_goods_amount_ranges == nullptr) {
            return false;
        }

        if (other.amount_ranges == nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_amount_ranges[i].min = 1;
                new_goods_amount_ranges[i].max = 0;
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_amount_ranges[i] = other.amount_ranges[i];
            }
        }
        this->amount_ranges = new_goods_amount_ranges;

        // amount
        size_t * new_goods_amount = new size_t[goods_count];
        if (new_goods_amount == nullptr) {
            return false;
        }

        if (other.amounts == nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_amount[i] = 0;
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_amount[i] = other.amounts[i];
            }
        }
        this->amounts = new_goods_amount;

        return true;
    }

    bool copy_invoice(const GoodsInvoice & other) {
        assert(other.count != 0);
        assert(this->count == other.count);
        size_t goods_count = other.count;
        assert(this->auto_release);
        this->auto_release = true;
        this->count = goods_count;

        // price
        if (other.prices != nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                this->prices[i] = other.prices[i];
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                this->prices[i] = 0.0;
            }
        }

        // amount
        if (other.amount_ranges == nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                this->amount_ranges[i].min = 1;
                this->amount_ranges[i].max = 0;
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                this->amount_ranges[i] = other.amount_ranges[i];
            }
        }

        // amount
        if (other.amounts == nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                this->amounts[i] = 0;
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                this->amounts[i] = other.amounts[i];
            }
        }

        return true;
    }

    void construct_copy(const GoodsInvoice & other) {
        if (other.count != 0) {
            this->create_new_invoice(other);
        }
    }

    bool internal_copy(const GoodsInvoice & other) {
        bool success;
        if (other.count != 0) {
            if (other.count != this->count || !this->auto_release) {
                this->destroy();
                success = this->create_new_invoice(other);
            }
            else {
                success = this->copy_invoice(other);
            }
        }
        else {
            this->destroy();
            success = true;
        }
        return success;
    }

    bool copy(const GoodsInvoice & other) {
        if (&other != this) {
            return this->internal_copy(other);
        }

        return false;
    }

    bool clone(const GoodsInvoice & other) {
        if (&other != this) {
            if (other.auto_release) {
                assert(other.auto_release);
                return this->internal_copy(other);
            }
            else {
                assert(!other.auto_release);
                this->auto_release  = other.auto_release;
                this->count         = other.count;
                this->prices        = other.prices;
                this->amount_ranges = other.amount_ranges;
                this->amounts       = other.amounts;
                return true;
            }
        }
    }

    void set_price_amount(size_t goods_count, double goods_prices[],
                          AmountRange goods_amount_ranges[] = nullptr,
                          size_t goods_amounts[] = nullptr) {
        this->destroy();

        this->auto_release  = false;
        this->count         = goods_count;
        this->prices        = goods_prices;
        this->amount_ranges = goods_amount_ranges;
        this->amounts       = goods_amounts;
    }

    bool create_price_amount(const GoodsInvoice & invoice) {
        bool result = this->copy(invoice);
        return result;
    }
};

class InvoiceBalance
{
private:
    double          total_price_;
    double          fluctuation_;
    double          min_price_error_;

    size_t          goods_count_;

    GoodsInvoice    in_invoice_;
    GoodsInvoice    invoice_;
    GoodsInvoice    best_answer_;

public:
    InvoiceBalance()
        : total_price_(0.0), fluctuation_(0.0),
          min_price_error_(std::numeric_limits<double>::max()), goods_count_(0) {
    }

    InvoiceBalance(double total_price, double fluctuation)
        : total_price_(total_price), fluctuation_(fluctuation),
          min_price_error_(std::numeric_limits<double>::max()), goods_count_(0) {
    }

    virtual ~InvoiceBalance() {
    }

    void set_total_price(double total_price, double fluctuation) {
        this->total_price_ = total_price;
        this->fluctuation_ = fluctuation;
    }

    bool set_price_amount(size_t goods_count, double goods_prices[], AmountRange amount_ranges[]) {
        this->goods_count_ = goods_count;
        this->in_invoice_.set_price_amount(goods_count, goods_prices, amount_ranges);
        bool result = this->invoice_.copy(this->in_invoice_);
        return result;
    }

    bool normalize() {
        bool result = true;
        this->fluctuation_ = round_price(this->fluctuation_);
        for (size_t i = 0; i < this->in_invoice_.count; i++) {
            this->in_invoice_.prices[i] = round_price(this->in_invoice_.prices[i]);
            if (this->in_invoice_.prices[i] < 0.0) {
                result = false;
            }
        }
        return result;
    }

private:
    double calc_total_price(size_t goods_count, const double goods_prices[], const size_t goods_amounts[]) {
        double actual_total_price = 0.0;
        for (size_t i = 0; i < goods_count; i++) {
            double money = round_price(goods_prices[i] * goods_amounts[i]);
            actual_total_price += money;
        }
        return actual_total_price;
    }

    double calc_total_price(const GoodsInvoice & invoice) {
        return calc_total_price(invoice.count, invoice.prices, invoice.amounts);
    }

    double calc_min_total_price(size_t idx, double total_price, const GoodsInvoice & invoice) {
        double actual_total_price = 0.0;
        for (size_t i = 0; i < invoice.count; i++) {
            if (idx == i) continue;
            int min_amount = invoice.amount_ranges[i].min;
            min_amount = (min_amount > 0) ? min_amount : 1;
            double money = round_price(invoice.prices[i] * min_amount);
            actual_total_price += money;
        }
        return actual_total_price;
    }

    int recalc_max_goods_amount(const GoodsInvoice & invoice, size_t idx) {
        double actual_total_price = calc_total_price(invoice);
        double min_total_price = calc_min_total_price(idx, this->total_price_, invoice);
        return (int)((this->total_price_ - actual_total_price - min_total_price) /
                     (this->invoice_.prices[idx] + this->fluctuation_));
    }

    void shuffle_goods_order(size_t goods_count, size_t goods_order[]) {
        for (ptrdiff_t i = goods_count - 1; i >= 1; i--) {
            ptrdiff_t idx = (ptrdiff_t)next_random_i64(0, i);
            assert(idx >= 0 && idx < (ptrdiff_t)goods_count);
            if (idx != i) {
                std::swap(goods_order[i], goods_order[idx]);
            }
        }
    }

    size_t find_padding_idx(size_t goods_count, size_t goods_amounts[]) {
        size_t count = 0;
        size_t padding_idx = size_t(-1);
        for (size_t i = 0; i < goods_count; i++) {
            if (goods_amounts[i] == 0.0) {
                if (count == 0)
                    padding_idx = i;
                count++;
            }
        }
        return ((count == 1) ? padding_idx : size_t(-1));
    }

    bool record_min_price_error(double price_error, const GoodsInvoice & invoice) {
        bool is_better = false;
        if (abs(price_error) < abs(this->min_price_error_)) {
            this->min_price_error_ = price_error;
            this->best_answer_ = invoice;
            is_better = true;
        }
        return is_better;
    }

    int adjust_price_and_amount(double total_price, double fluctuation,
                                GoodsInvoice & invoice) {
        int result = 0;
        size_t padding_idx = find_padding_idx(invoice.count, invoice.amounts);
        if (padding_idx == size_t(-1)) {
            return -1;
        }
        ptrdiff_t padding_amount = -1;
        double actual_total_price = calc_total_price(invoice);
        if (actual_total_price <= total_price) {
            padding_amount = (ptrdiff_t)((total_price - actual_total_price) / invoice.prices[padding_idx]);
            invoice.amounts[padding_idx] = padding_amount;
            if (padding_amount <= 0 || padding_amount < (ptrdiff_t)invoice.amount_ranges[padding_idx].min) {
                return -1;
            }
        }
        else {
            return -1;
        }

        actual_total_price = calc_total_price(invoice);
        double actual_price_error = actual_total_price - total_price;
        record_min_price_error(actual_price_error, invoice);

        GoodsInvoice test_invoice(invoice);
        for (size_t i = 0; i < test_invoice.count; i++) {
            double price_adjust = round_price(-actual_price_error / test_invoice.prices[i]);
            test_invoice.prices[i] += price_adjust;
            actual_total_price = calc_total_price(test_invoice);
            double price_error = actual_total_price - total_price;
            record_min_price_error(price_error, test_invoice);
            test_invoice = invoice;
        }

        return result;
    }

    bool search_price_and_amount() {
        bool solvable = false;

        double total_price = this->total_price_;
        double fluctuation = this->fluctuation_;
        size_t goods_count = this->invoice_.count;

        size_t search_cnt = 0;
        double price_error = std::numeric_limits<double>::max();
        double min_price_error = std::numeric_limits<double>::max();
        size_t * goods_order = new size_t[goods_count];
        if (goods_order == nullptr)
            return false;

        while (price_error != 0.0) {
            bool retry_next = false;
            for (size_t i = 0; i < goods_count; i++) {
                double price_change = normal_dist_random_f(-this->fluctuation_, this->fluctuation_);
                this->invoice_.prices[i] = round_price(this->in_invoice_.prices[i] + price_change);
            }

            for (size_t i = 0; i < goods_count; i++) {
                this->invoice_.amounts[i] = 0;
                goods_order[i] = i;
            }

            // shuffle goods_order[]
            shuffle_goods_order(goods_count, goods_order);
            
            for (ptrdiff_t i = goods_count - 1; i >= 1; i--) {
                size_t idx = goods_order[i];
                assert(this->invoice_.amounts[idx] == 0.0);
                int min_goods_amount = this->invoice_.amount_ranges[idx].min;
                int max_goods_amount = this->invoice_.amount_ranges[idx].max;
                int actual_max_goods_amount = recalc_max_goods_amount(this->invoice_, idx);
                min_goods_amount = (min_goods_amount > 0) ? min_goods_amount : 1;
                if (max_goods_amount > 0 && max_goods_amount >= min_goods_amount)
                    max_goods_amount = (max_goods_amount < actual_max_goods_amount) ? max_goods_amount : actual_max_goods_amount;
                else
                    max_goods_amount = actual_max_goods_amount;
                retry_next = (min_goods_amount > max_goods_amount);
                if (retry_next) {
                    break;
                }
                int rand_amount = normal_dist_random_i32(min_goods_amount, max_goods_amount);
                assert(rand_amount >= min_goods_amount);
                this->invoice_.amounts[idx] = rand_amount;                
            }

            if (!retry_next) {
                int result = adjust_price_and_amount(total_price, fluctuation, this->invoice_);
                if (result == 0) {
                    //
                }
            }

            search_cnt++;
            if (abs(this->min_price_error_) <= 0.0000001) {
                solvable = true;
                break;
            }
            if (search_cnt > 1000000) {
                break;
            }
        }

        if (goods_order != nullptr) {
            delete [] goods_order;
        }

        printf(" search_cnt = %u\n\n", (uint32_t)search_cnt);
        return solvable;
    }

    void display_best_answer() {
        printf("\n");
        printf("   #        amount         price           money\n");
        printf("---------------------------------------------------------------\n\n");
        double actual_total_price = calc_total_price(this->best_answer_);
        for (size_t i = 0; i < this->best_answer_.count; i++) {
            printf("  %2u     %8u       %8.2f       %10.2f\n",
                   (uint32_t)(i + 1),
                   (uint32_t)this->best_answer_.amounts[i],
                   this->best_answer_.prices[i],
                   this->best_answer_.moneys(i));
        }
        printf("\n");
        printf(" Total                                 %10.2f\n", actual_total_price);
        printf("---------------------------------------------------------------\n");
        printf(" Error                                 %10.2f\n", (actual_total_price - this->total_price_));
        printf("\n\n");
        printf("---------------------------------------------------------------\n");
        printf(" The best price error:  %0.16f\n", this->min_price_error_);
        printf("---------------------------------------------------------------\n\n");
    }

public:
    int solve() {
        this->normalize();

        bool solvable = search_price_and_amount();
        if (solvable) {
            printf(" Found a perfect answer.\n\n");
        }
        else {
            printf(" Not found a perfect answer.\n\n");
        }

        this->display_best_answer();
        return (solvable ? 0 : 1);
    }
};

double strToDouble(const std::string & value, double default_value)
{
    if (value.empty() || value.c_str() == nullptr || value == "")
        return default_value;
    else
        return std::atof(value.c_str());
}

bool parse_price_range(const std::string & value, int & price_min, int & price_max)
{
    bool is_ok = false;
    size_t pos, start;
    int value_min, value_max;
    start = IniFile::skip_whitespace_chars(value);
    if (start != std::string::npos) {
        pos = IniFile::find_char(value, start, '-');
        if (pos != std::string::npos) {
            std::string str_min, str_max;
            // range: min
            IniFile::copy_string(value, str_min, start, pos);
            if (str_min.size() > 0 && !str_min.empty()) {
                value_min = std::atoi(str_min.c_str());
                if (value_min > 0) {
                    price_min = value_min;
                    is_ok = true;
                }
            }
            // range: max
            IniFile::copy_string(value, str_max, pos + 1, value.size());
            if (str_max.size() > 0 && !str_max.empty()) {
                value_max = std::atoi(str_max.c_str());
                if (value_max > 0) {
                    price_max = value_max;
                }
            }
        }
        else {
            // range: min
            value_min = std::atoi(value.c_str() + start);
            if (value_min > 0) {
                price_min = value_min;
                is_ok = true;
            }
        }
    }
    return is_ok;
}

size_t read_config_value(IniFile & config)
{
    std::string value;

    // TotalPrice
    if (config.contains("TotalPrice")) {
        value = config.values("TotalPrice");
        s_total_price = strToDouble(value, kDefaultTotalPrice);
    }
    else {
        s_total_price = kDefaultTotalPrice;
    }

    // Fluctuation
    if (config.contains("Fluctuation")) {
        value = config.values("Fluctuation");
        s_fluctuation = strToDouble(value, kDefaultFluctuation);
    }
    else {
        s_fluctuation = kDefaultFluctuation;
    }

    if (s_goods_prices != nullptr) {
        delete [] s_goods_prices;
    }
    if (s_goods_amount_ranges != nullptr) {
        delete [] s_goods_amount_ranges;
    }

    s_goods_price_count = 0;
    s_goods_prices = new double[kMaxPriceCount];
    if (s_goods_prices == nullptr)
        return size_t(-1);

    for (size_t i = 0; i < kMaxPriceCount; i++) {
        s_goods_prices[i] = 0.0;
    }

    s_goods_amount_ranges = new AmountRange[kMaxPriceCount];
    if (s_goods_amount_ranges == nullptr)
        return size_t(-1);

    for (size_t i = 0; i < kMaxPriceCount; i++) {
        s_goods_amount_ranges[i].min = 1;
        s_goods_amount_ranges[i].max = 0;
    }

    // Price list
    size_t price_count = 0;
    for (size_t i = 0; i < kMaxPriceCount; i++) {
        std::string price_name = "Price";
        std::string range_name = "Range";
        size_t index = i + 1;
        if (index < 10) {
            price_name.push_back((char)index + '0');
            range_name.push_back((char)index + '0');
        }
        else {
            price_name.push_back((char)(index / 10) + '0');
            price_name.push_back((char)(index % 10) + '0');
            range_name.push_back((char)(index / 10) + '0');
            range_name.push_back((char)(index % 10) + '0');
        }

        // Price ##
        if (config.contains(price_name)) {
            value = config.values(price_name);
            double goods_price = strToDouble(value, 0.0);
            if (goods_price != 0.0 && goods_price != NAN) {
                if (price_count < kMaxPriceCount) {
                    s_goods_prices[price_count] = round_price(goods_price);
                    // Range ##
                    int price_min = 1, price_max = 0;
                    if (config.contains(range_name)) {
                        value = config.values(range_name);
                        bool is_ok = parse_price_range(value, price_min, price_max);
                        if (is_ok) {
                            s_goods_amount_ranges[price_count].min = price_min;
                            s_goods_amount_ranges[price_count].max = price_max;
                        }
                    }
                }
                price_count++;
            }
        }
    }

    s_goods_price_count = price_count;
    return price_count;
}

void app_shutdown()
{
    if (s_goods_prices != nullptr) {
        delete [] s_goods_prices;
        s_goods_prices = nullptr;
    }
    if (s_goods_amount_ranges != nullptr) {
        delete [] s_goods_amount_ranges;
        s_goods_amount_ranges = nullptr;
    }
}

int main(int argc, char * argv[])
{
    ::srand((unsigned int)::time(NULL));
    printf("\n");

    size_t nPriceCount = size_t(-1);

    IniFile iniFile;
    int nReadStatus = iniFile.open("Invoice.txt");
    printf(" IniFile('Invoice.txt'): nReadStatus = %d\n\n", nReadStatus);
    if (nReadStatus == 0) {
        int nParseCount = iniFile.parse();
        if (nParseCount > 0) {
            nPriceCount = read_config_value(iniFile);
        }
    }

    InvoiceBalance invoiceBalance;
    if (nPriceCount != size_t(-1)) {
        invoiceBalance.set_total_price(s_total_price, s_fluctuation);
        invoiceBalance.set_price_amount(s_goods_price_count, s_goods_prices, s_goods_amount_ranges);
    }
    else {
        invoiceBalance.set_total_price(kDefaultTotalPrice, kDefaultFluctuation);
        invoiceBalance.set_price_amount(_countof(default_goods_prices), default_goods_prices, default_goods_amount_ranges);
    }

    int result = invoiceBalance.solve();
    app_shutdown();
#if defined(_MSC_VER)
    ::system("pause");
#endif
    return result;
}
