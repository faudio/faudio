
#include <algorithm>
#include <gtest/gtest.h>
#include <scl/utility.hpp>

TEST(AudioUtility, RawBuffer)
{
  scl::raw_buffer b;
  scl::raw_buffer b2 = std::move(b);
}

TEST(AudioUtility, RawDump)
{
  scl::raw_buffer b;
  scl::raw_buffer c;
  b.reset(256);
  c.reset(256);
  int acc = 0;
  auto accum = [&acc]() -> int { return acc++; };
  auto succ = [](int x) -> int { return x + 1; };
  std::generate(b.begin(), b.end(), accum);
  std::transform(b.begin(), b.end(), c.begin(), succ);
  scl::raw_dump(b, false);
  scl::raw_dump(c, false);
}

