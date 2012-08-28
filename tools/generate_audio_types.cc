
#include <boost/format.hpp>
#include <boost/program_options.hpp>
#include <scl/audio/audio_types.hpp>

/*
  This program generates all possible combinations of audio types (for testing purposes).

    $ scl_generate_audio_types -n 3

      ==================================================
      Current generation: 1
      Number of types:    3
      --------------------------------------------------
      [sample64]
      <sample64 x 512>
      (sample64, sample64)

      ==================================================
      Current generation: 2
      Number of types:    15
      --------------------------------------------------
      [[sample64]]
      <[sample64] x 512>
      [<sample64 x 512>]
      <<sample64 x 512> x 512>
      [(sample64, sample64)]
      <(sample64, sample64) x 512>
      ([sample64], [sample64])

    etc.
*/

using namespace scl::audio;

static int  num_generations    = 2;
static bool print_test_routine = false;
static bool print_declaration  = false;
static bool normal_output      = true;
static bool include_info       = false;

inline std::list<audio_type> first_generation()
{
  std::list<audio_type> gen;
  gen.push_back(audio_type::sample32());
  gen.push_back(audio_type::sample64());
  return gen;
}

inline std::list<audio_type> next_generation(std::list<audio_type> prev, int max_level)

{
  std::list<audio_type> next;
  for (audio_type type : prev)
  {
    if (type.levels() < max_level)
    {
      next.push_back(audio_type::list(type));
      static constexpr size_t powers[] = { 512 };
      for (size_t p : powers)
        next.push_back(audio_type::vector(type, p));
    }
  }
  for (audio_type type : prev)
  {
    for (audio_type type2 : prev)
    {
      if ((type.levels() < max_level) && (type2.levels() < max_level))
        next.push_back(audio_type::pair(type, type2));
    }
  }
  return next;
}

void print_generation(int gen, int size)
{
  if (print_test_routine) std::cout << "// ";
  std::cout << "==================================================\n";
  if (print_test_routine) std::cout << "// ";
  std::cout << "Current generation: " << gen << "\n";
  if (print_test_routine) std::cout << "// ";
  std::cout << "Number of types:    " << size << "\n";
  if (print_test_routine) std::cout << "// ";
  std::cout << "--------------------------------------------------\n";
}

#define print_normal(X) \
  if (normal_output) { std::cout << X; }

inline std::list<audio_type> generate(int max_level)
{
  std::list<audio_type> all;
  std::list<audio_type> gen;
  gen = first_generation();
  gen = next_generation(gen, max_level);
  
  int gen_count = 0;
  do
  {
    print_generation(++gen_count, gen.size());

    for (auto t : gen)
    {
      if (print_test_routine)
      {
        print_normal(boost::format("\n"));
        print_normal(boost::format("print_name (\" %s \");\n") % t.declaration());
        print_normal(boost::format("print_align < %s >();\n") % t.declaration());
        if (t.is_pair())
          print_normal(boost::format("print_offset< %s >();\n") % t.declaration());
      }
      else if (print_declaration)
      {
        print_normal(t.declaration() << "\n");
      }
      else
      {
        print_normal(t << "\n");
      }

      if (include_info)
      {
        print_normal(boost::format("  Size:      %d\n") % t.size());
        print_normal(boost::format("  Alignment: %d\n") % t.align());
        print_normal(boost::format("  Offset:    %d\n") % t.offset());
      }
    }
    std::cout << "\n";
    all.insert(all.end(), gen.begin(), gen.end());
    gen = next_generation(gen, max_level);
  }
  while (gen.size() > 0);
  return all;
}

void print_usage(boost::program_options::options_description desc)
{
  std::cout << "Usage: scl_generate_audio_types [options]\n";
  std::cout << desc << "\n";
}

int main(int argc, char const* argv[])
{
  using namespace boost::program_options;
  options_description desc("Allowed options");
  desc.add_options()
  ("help,h",  "Print help")
  ("depth,n",  value<int>(),
   "Maximum level of nesting")
  ("count,C", "Display number of elements in each generation")
  ("info,i",  "Include type information")
  ("cpp",     "Generate C++ syntax")
  ("test",    "Generate C++ test code")
  ;
  variables_map vm;
  store(parse_command_line(argc, argv, desc), vm);
  notify(vm);
  if (vm.count("cpp"))
    print_declaration = true;
  if (vm.count("test"))
    print_test_routine = true;
  if (vm.count("count"))
    normal_output = false;
  if (vm.count("info"))
    include_info = true;
  if (vm.count("depth"))
    num_generations = vm["depth"].as<int>();
  if (vm.count("help"))
  {
    print_usage(desc);
    return 1;
  }
  generate(num_generations);
  return 0;
}