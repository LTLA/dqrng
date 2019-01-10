// Copyright 2018 Ralf Stubner (daqana GmbH)
//
// This file is part of dqrng.
//
// dqrng is free software: you can redistribute it and/or modify it
// under the terms of the GNU Affero General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// dqrng is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with dqrng.  If not, see <http://www.gnu.org/licenses/>.

#ifndef DQRNG_GENERATOR_H
#define DQRNG_GENERATOR_H 1

#include <cstdint>
#include <chrono>
#include <memory>
#include <boost/random/mersenne_twister.hpp>
#include <boost/random/random_device.hpp>
#include <type_traits>

// explicit specialisations while https://github.com/boostorg/random/pull/24 is not merged
namespace boost {
namespace random {
/** Returns the smallest value that the generator can produce. */
template <>
BOOST_CONSTEXPR mt19937_64::result_type mt19937_64::min BOOST_PREVENT_MACRO_SUBSTITUTION ()
{ return 0; }
/** Returns the largest value that the generator can produce. */
template <>
BOOST_CONSTEXPR mt19937_64::result_type mt19937_64::max BOOST_PREVENT_MACRO_SUBSTITUTION ()
{ return boost::low_bits_mask_t<64>::sig_bits; }
} // random
} // boost


namespace dqrng {
// conservative default
using default_64bit_generator = boost::random::mt19937_64;

class random_64bit_generator {
public:
  typedef uint64_t result_type;

  virtual ~random_64bit_generator() {};
  virtual result_type operator() () = 0;
  virtual void seed(result_type seed) = 0;
  result_type min() {return 0.0;};
  result_type max() {return UINT64_MAX;};
};

using rng64_t = std::shared_ptr<random_64bit_generator>;

template<typename RNG>
class random_64bit_wrapper : public random_64bit_generator {
  static_assert(std::is_same<random_64bit_generator::result_type, typename RNG::result_type>::value,
                "Provided RNG has wrong result_type");
  static_assert(RNG::max() == UINT64_MAX, "Provided RNG has wrong maximum.");
  static_assert(RNG::min() == 0, "Provided RNG has wrong minimum.");
private:
  RNG gen;

public:
  random_64bit_wrapper() : gen() {};
  random_64bit_wrapper(result_type seed) : gen(seed) {};
  virtual result_type operator() () {return gen();}
  virtual void seed(result_type seed) {gen.seed(seed);}
};

template<typename RNG = default_64bit_generator>
typename std::enable_if<!std::is_base_of<random_64bit_generator, RNG>::value, rng64_t>::type
generator (uint64_t seed) {
  return std::make_shared<random_64bit_wrapper<RNG>>(seed);
}

template<typename RNG = default_64bit_generator>
typename std::enable_if<std::is_base_of<random_64bit_generator, RNG>::value, rng64_t>::type
generator (uint64_t seed) {
  return std::make_shared<RNG>(seed);
}

template<typename RNG = default_64bit_generator>
rng64_t generator() {
  uint64_t seed = boost::random::random_device{}() |
    std::chrono::system_clock::now().time_since_epoch().count();
  return generator<RNG>(seed);
}
} // namespace dqrng

#endif // DQRNG_GENERATOR_H
