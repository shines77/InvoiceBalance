
#if defined(_MSC_VER) && defined(_DEBUG)
#include <vld.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stddef.h>
#include <math.h>
#include <time.h>
#include <assert.h>

#include <limits>
#include <algorithm>

#include "CountOf.h"
#include "IniFile.h"

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

static const double kDefaultTotalPrice = 120000.0;
static const double kDefaultFluctuation = 2.0;

static const size_t kMaxGoodsCount = 20;

static double default_goods_prices[] = {
    212.00,
    172.5,
    226.0
};

static AmountRange default_goods_amount_range[] = {
    { 100, 0 },
    { 100, 0 },
    { 100, 0 }
};

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

uint64_t next_random64()
{
    return (((uint64_t)next_random32() << 32) | (uint64_t)next_random32());
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
        return (min_num + (next_random64() % int64_t(max_num - min_num + 1)));
    else if (min_num > max_num)
        return (max_num + (next_random64() % int64_t(min_num - max_num + 1)));
    else
        return min_num;
}

size_t next_random_64(size_t min_num, size_t max_num)
{
    if (min_num < max_num)
        return (min_num + (next_random64() % size_t(max_num - min_num + 1)));
    else if (min_num > max_num)
        return (max_num + (next_random64() % size_t(min_num - max_num + 1)));
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

size_t normal_dist_random_64(size_t min_num, size_t max_num)
{
    double randomf = normal_dist_next_random();

    if (min_num < max_num)
        return (min_num + size_t(randomf * (max_num - min_num)));
    else if (min_num > max_num)
        return (max_num + size_t(randomf * (min_num - max_num)));
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
    size_t *        amounts;
    AmountRange *   amount_ranges;

    GoodsInvoice() : auto_release(false), count(0),
                     prices(nullptr), amounts(nullptr), amount_ranges(nullptr) {
    }

    GoodsInvoice(size_t goods_count, double goods_prices[],
                 AmountRange goods_amount_ranges[] = nullptr,
                 size_t goods_amounts[] = nullptr)
        : auto_release(false), count(goods_count),
          prices(goods_prices), amounts(goods_amounts), amount_ranges(goods_amount_ranges) {
    }

    GoodsInvoice(const GoodsInvoice & other)
        : auto_release(false), count(0),
          prices(nullptr), amounts(nullptr), amount_ranges(nullptr) {
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
            if (this->amounts) {
                delete[] this->amounts;
                this->amounts = nullptr;
            }
            if (this->amount_ranges) {
                delete[] this->amount_ranges;
                this->amount_ranges = nullptr;
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
            this->count         = other.size();
            this->prices        = other.prices;
            this->amounts       = other.amounts;
            this->amount_ranges = other.amount_ranges;
        }
    }

    bool create_new_invoice(const GoodsInvoice & other) {
        assert(other.size() != 0);
        size_t goods_count = other.size();
        this->auto_release = true;
        this->count = goods_count;

        // price
        double * new_goods_prices = new double[goods_count];
        if (new_goods_prices == nullptr) {
            return false;
        }

        if (other.prices != nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_prices[i] = other.prices[i];
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_prices[i] = 0.0;
            }
        }
        this->prices = new_goods_prices;

        // amount
        size_t * new_goods_amounts = new size_t[goods_count];
        if (new_goods_amounts == nullptr) {
            return false;
        }

        if (other.amounts == nullptr) {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_amounts[i] = 0;
            }
        }
        else {
            for (size_t i = 0; i < goods_count; i++) {
                new_goods_amounts[i] = other.amounts[i];
            }
        }
        this->amounts = new_goods_amounts;

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

        return true;
    }

    bool copy_invoice(const GoodsInvoice & other) {
        assert(other.size() != 0);
        assert(this->count == other.size());
        size_t goods_count = other.size();
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

        // amount range
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

        return true;
    }

    void construct_copy(const GoodsInvoice & other) {
        if (other.size() != 0) {
            this->create_new_invoice(other);
        }
    }

    bool internal_copy(const GoodsInvoice & other) {
        bool success;
        if (other.size() != 0) {
            if (other.size() != this->count || !this->auto_release) {
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
                this->count         = other.size();
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

    bool create_price_amount(const GoodsInvoice & goods_list) {
        bool result = this->copy(goods_list);
        return result;
    }
};

struct Goods {
    double          price;
    size_t          amount;
    AmountRange     amount_range;

    Goods() : price(0.0), amount(0) {
    }

    double total_money() const {
        return round_price(this->price * this->amount);
    }
};

class InvoiceBalance
{
public:
    typedef std::vector<Goods>  GoodsList;

private:    
    double  total_price_;
    double  fluctuation_;
    double  min_price_error_;

    size_t  goods_count_;

    GoodsList  input_goods_;
    GoodsList  goods_list_;
    GoodsList  best_answer_;

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

    void set_price_amount(const GoodsList & goods_list) {
        this->input_goods_ = goods_list;
        this->goods_list_.resize(goods_list.size());
    }

    bool normalize_prices() {
        bool result = true;
        this->fluctuation_ = round_price(this->fluctuation_);
        for (size_t i = 0; i < this->input_goods_.size(); i++) {
            this->input_goods_[i].price = round_price(this->input_goods_[i].price);
            if (this->input_goods_[i].price < 0.0) {
                result = false;
            }
        }
        return result;
    }

private:
    double calc_total_price(const GoodsList & goods_list) {
        double actual_total_price = 0.0;
        for (size_t i = 0; i < goods_list.size(); i++) {
            double total = round_price(goods_list[i].price * goods_list[i].amount);
            actual_total_price += total;
        }
        return actual_total_price;
    }

    double calc_min_total_price(size_t idx, double total_price, const GoodsList & goods_list) {
        double actual_total_price = 0.0;
        for (size_t i = 0; i < goods_list.size(); i++) {
            if (i == idx) continue;
            int min_amount = (std::max)(goods_list[i].amount_range.min, 1);
            double money = round_price(goods_list[i].price * min_amount);
            actual_total_price += money;
        }
        return actual_total_price;
    }

    int recalc_max_goods_amount(const GoodsList & goods_list, size_t idx) {
        double actual_total_price = calc_total_price(goods_list);
        double min_total_price = calc_min_total_price(idx, this->total_price_, goods_list);
        return (int)((this->total_price_ - actual_total_price - min_total_price) /
                     (this->input_goods_[idx].price - this->fluctuation_));
    }

    void shuffle_goods_order(size_t goods_count, std::vector<size_t> & goods_orders) {
        for (ptrdiff_t i = goods_count - 1; i >= 1; i--) {
            ptrdiff_t idx = (ptrdiff_t)next_random_i64(0, i);
            assert(idx >= 0 && idx < (ptrdiff_t)goods_count);
            if (idx != i) {
                std::swap(goods_orders[i], goods_orders[idx]);
            }
        }
    }

    size_t find_unique_padding_idx(const GoodsList & goods_list) {
        size_t count = 0;
        size_t padding_idx = size_t(-1);
        for (size_t i = 0; i < goods_list.size(); i++) {
            if (goods_list[i].amount == 0.0) {
                if (count == 0)
                    padding_idx = i;
                count++;
            }
        }
        return ((count == 1) ? padding_idx : size_t(-1));
    }

    bool record_min_price_error(double price_error, const GoodsList & goods_list) {
        bool is_better = false;
        if (abs(price_error) < this->min_price_error_) {
            this->min_price_error_ = abs(price_error);
            this->best_answer_ = goods_list;
            is_better = true;
        }
        return is_better;
    }

    int adjust_price_and_amount(double total_price, double fluctuation,
                                GoodsList & goods_list) {
        int result = 0;
        size_t padding_idx = find_unique_padding_idx(goods_list);
        if (padding_idx == size_t(-1)) {
            return -1;
        }
        ptrdiff_t padding_amount = -1;
        double actual_total_price = calc_total_price(goods_list);
        if (actual_total_price <= total_price) {
            padding_amount = (ptrdiff_t)((total_price - actual_total_price) / goods_list[padding_idx].price);
            goods_list[padding_idx].amount = padding_amount;
            if (padding_amount <= 0 || padding_amount < (ptrdiff_t)goods_list[padding_idx].amount_range.min) {
                return -1;
            }
        }
        else {
            return -1;
        }

        actual_total_price = calc_total_price(goods_list);
        double actual_price_diff = actual_total_price - total_price;
        record_min_price_error(actual_price_diff, goods_list);

        for (size_t i = 0; i < goods_list.size(); i++) {
            double price_adjust = round_price(actual_price_diff / goods_list[i].price);
            double old_price = goods_list[i].price;
            goods_list[i].price -= price_adjust;
            double price_diff = actual_total_price - price_adjust * goods_list[i].amount - total_price;
            record_min_price_error(price_diff, goods_list);
            goods_list[i].price = old_price;
        }

        return result;
    }

    bool search_price_and_amount() {
        bool solvable = false;

        double total_price = this->total_price_;
        double fluctuation = this->fluctuation_;
        size_t goods_count = this->goods_list_.size();

        size_t search_cnt = 0;
        double price_error = std::numeric_limits<double>::max();
        double min_price_error = std::numeric_limits<double>::max();
        std::vector<size_t> goods_orders;
        goods_orders.resize(goods_count);

        while (price_error != 0.0) {
            bool retry_next = false;
            for (size_t i = 0; i < goods_count; i++) {
                double price_change = normal_dist_random_f(-this->fluctuation_, this->fluctuation_);
                this->goods_list_[i].price = round_price(this->input_goods_[i].price + price_change);
            }

            for (size_t i = 0; i < goods_count; i++) {
                this->goods_list_[i].amount = 0;
                goods_orders[i] = i;
            }

            // shuffle goods_order[]
            shuffle_goods_order(goods_count, goods_orders);
            
            for (ptrdiff_t i = goods_count - 1; i >= 1; i--) {
                size_t idx = goods_orders[i];
                assert(this->goods_list_[idx].amount == 0.0);
                int min_amount = this->goods_list_[idx].amount_range.min;
                int max_amount = this->goods_list_[idx].amount_range.max;
                int actual_max_goods_amount = recalc_max_goods_amount(this->goods_list_, idx);
                min_amount = (std::max)(min_amount, 1);
                if (max_amount >= min_amount)
                    max_amount = (std::min)(max_amount, actual_max_goods_amount);
                else
                    max_amount = actual_max_goods_amount;
                retry_next = (min_amount > max_amount);
                if (retry_next) {
                    break;
                }
                int rand_amount = normal_dist_random_i32(min_amount, max_amount);
                assert(rand_amount >= min_amount);
                this->goods_list_[idx].amount = rand_amount;                
            }

            if (!retry_next) {
                int result = adjust_price_and_amount(total_price, fluctuation, this->goods_list_);
                if (result == 0) {
                    // Adjust success, have no overflow
                }
            }

            search_cnt++;
            if (this->min_price_error_ <= 0.0000001) {
                solvable = true;
                break;
            }
            if (search_cnt > 1000000) {
                break;
            }
        }

        printf(" search_cnt = %u\n\n", (uint32_t)search_cnt);
        return solvable;
    }

    bool fast_search_price_and_amount() {
        bool solvable = false;

        double total_price = this->total_price_;
        double fluctuation = this->fluctuation_;
        size_t goods_count = this->goods_list_.size();
        size_t n = this->goods_list_.size();

        std::vector<double> result;
        std::vector<double> remains;
        result.reserve(n);
        remains.reserve(n);

        double sum = 0.0;
        for (size_t i = 0; i < n; i++) {
            sum += this->goods_list_[i].price;
            result.push_back(this->goods_list_[i].price);
            this->goods_list_[i].amount = 1;
        }

        double remain = 0;
        for (intptr_t i = n - 1; i >= 0; i--) {
            remain += this->goods_list_[i].price * this->goods_list_[i].amount;
            remains.push_back(remain);
        }

        size_t search_cnt = 0;
        do {
            double balance = total_price;
            for (size_t i = 0; i < n; i++) {
                intptr_t max_count = (intptr_t)((balance - remains[i]) / this->goods_list_[i].price);
                if (max_count <= 0) {
                    search_cnt++;
                    continue;
                }
                size_t count = normal_dist_random_64(1, (size_t)max_count);
                this->goods_list_[i].amount = count;
                balance -= this->goods_list_[i].price * count;
            }

            double diff = balance;
            if (diff < 0.0) {
                diff = -diff;
                // Add
                for (size_t i = 0; i < n; i++) {
                    double change = (std::min)(fluctuation, diff / this->goods_list_[i].amount);
                    double new_price = result[i];
                    new_price += change;
                    new_price = round_price(new_price);
                    change = new_price - result[i];
                    result[i] = new_price;
                    diff -= change * this->goods_list_[i].amount;
                }
            } else {
                // Sub
                for (size_t i = 0; i < n; i++) {
                    double change = (std::min)(fluctuation, diff / this->goods_list_[i].amount);
                    double new_price = result[i];
                    new_price -= change;
                    new_price = round_price(new_price);
                    change = new_price - result[i];
                    result[i] = new_price;
                    diff -= change * this->goods_list_[i].amount;
                }
            }

            double actual_total = calc_total_price(this->goods_list_);
            double total_diff = actual_total - total_price;
            record_min_price_error(total_diff, this->goods_list_);

            if (abs(total_diff) <= 0.0000001) {
                solvable = true;
                break;
            }

            search_cnt++;
            if (search_cnt > 1000000) {
                break;
            }
        } while (1);

        printf(" search_cnt = %u\n\n", (uint32_t)search_cnt);
        return solvable;
    }

    void display_best_answer() {
        printf("\n");
        printf("   #        amount         price           money\n");
        printf("---------------------------------------------------------------\n\n");
        double actual_total_price = calc_total_price(this->best_answer_);
        for (size_t i = 0; i < this->best_answer_.size(); i++) {
            printf("  %2u     %8u       %8.2f       %10.2f\n",
                   (uint32_t)(i + 1),
                   (uint32_t)this->best_answer_[i].amount,
                   this->best_answer_[i].price,
                   this->best_answer_[i].total_money());
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
        this->normalize_prices();

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

    int solve_fast() {
        this->normalize_prices();

        bool solvable = fast_search_price_and_amount();
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
        return std::stof(value.c_str());
}

bool parse_amount_range(const std::string & value, int & min_val, int & max_val)
{
    bool is_ok = false;
    size_t pos, start;
    int min_value, max_value;
    try {
        start = IniFile::skip_whitespace_chars(value);
        if (start != std::string::npos) {
            pos = IniFile::find_char(value, start, '-');
            if (pos != std::string::npos) {
                std::string str_min, str_max;
                // range: min value
                IniFile::copy_string(value, str_min, start, pos);
                if (str_min.size() > 0 && !str_min.empty()) {
                    min_value = std::atoi(str_min.c_str());
                    if (min_value > 0) {
                        min_val = min_value;
                        is_ok = true;
                    }
                }
                // range: max value
                IniFile::copy_string(value, str_max, pos + 1, value.size());
                if (str_max.size() > 0 && !str_max.empty()) {
                    max_value = std::stoi(str_max.c_str());
                    if (max_value > 0) {
                        max_val = max_value;
                    }
                }
            }
            else {
                // range: min value
                min_value = std::stoi(value.c_str() + start);
                if (min_value > 0) {
                    min_val = min_value;
                    is_ok = true;
                }
            }
        }
    } catch (const std::invalid_argument & ex) {
        std::cout << "std::invalid_argument::what(): " << ex.what() << '\n';
    } catch (const std::out_of_range & ex) {
        std::cout << "std::out_of_range::what(): " << ex.what() << '\n';
    }
    return is_ok;
}

struct AppConfig {
    double total_price;
    double fluctuation;

    std::vector<Goods> goods;
};

size_t read_config_value(IniFile & iniFile, AppConfig & config)
{
    std::string value;

    // TotalPrice
    if (iniFile.contains("TotalPrice")) {
        value = iniFile.values("TotalPrice");
        config.total_price = strToDouble(value, kDefaultTotalPrice);
    }
    else {
        config.total_price = kDefaultTotalPrice;
    }

    // Fluctuation
    if (iniFile.contains("Fluctuation")) {
        value = iniFile.values("Fluctuation");
        config.fluctuation = strToDouble(value, kDefaultFluctuation);
    }
    else {
        config.fluctuation = kDefaultFluctuation;
    }

    // Price list
    size_t goods_count = 0;
    for (size_t i = 0; i < kMaxGoodsCount; i++) {
        std::string index_str = std::to_string(i + 1);

        std::string price_name = "Price";
        std::string range_name = "Range";
        price_name += index_str;
        range_name += index_str;

        // Price ##
        if (iniFile.contains(price_name)) {
            value = iniFile.values(price_name);
            double goods_price = strToDouble(value, 0.0);
            if (goods_price != 0.0 && goods_price != NAN) {
                Goods goods;
                goods.price = round_price(goods_price);
                // Range ##
                int range_min = 1, range_max = 0;
                if (iniFile.contains(range_name)) {
                    value = iniFile.values(range_name);
                    bool is_ok = parse_amount_range(value, range_min, range_max);
                    if (is_ok) {
                        // Read OK
                    }
                } 
                goods.amount_range.min = range_min;
                goods.amount_range.max = range_max;
                config.goods.push_back(goods);
                goods_count++;
            }
        }
    }

    return goods_count;
}

int main(int argc, char * argv[])
{
    ::srand((unsigned int)::time(NULL));

    AppConfig config;
    size_t nGoodsCount = size_t(-1);

    IniFile iniFile;
    int nReadStatus = iniFile.open("Invoice.txt");
    printf("\n");
    printf(" IniFile('Invoice.txt'): nReadStatus = %d\n\n", nReadStatus);
    if (nReadStatus == 0) {
        int nParseCount = iniFile.parse();
        if (nParseCount > 0) {
            nGoodsCount = read_config_value(iniFile, config);
        }
    }

    InvoiceBalance goods_listBalance;
    if (nGoodsCount != size_t(-1)) {
        goods_listBalance.set_total_price(config.total_price, config.fluctuation);
        goods_listBalance.set_price_amount(config.goods);
    }
    else {
        // Get the default prices and amount ranges
        std::vector<Goods> goods_list;
        size_t min_goods_count = (std::min)(_countof(default_goods_prices), _countof(default_goods_amount_range));
        for (size_t i = 0; i < min_goods_count; i++) {
            Goods goods;
            goods.price = default_goods_prices[i];
            goods.amount = 0;
            goods.amount_range = default_goods_amount_range[i];
            goods_list.push_back(goods);
        }
        goods_listBalance.set_total_price(kDefaultTotalPrice, kDefaultFluctuation);
        goods_listBalance.set_price_amount(goods_list);
    }

#if 1
    int result = goods_listBalance.solve();
#else
    int result = goods_listBalance.solve_fast();
#endif

#if defined(_MSC_VER) && defined(_DEBUG)
    ::system("pause");
#endif
    return result;
}
