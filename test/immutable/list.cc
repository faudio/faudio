// 
// 
// template<class List>
// List add_before(typename List::value_type x, List xs)
// {
//   List ys(xs);
//   ys.push_back(x);
//   return ys;
// }
// 
// template<>
// doremir::scl::ilist<int>
// add_before(doremir::scl::ilist<int>::value_type x,
//            doremir::scl::ilist<int> xs)
// {
//   return xs.add_before(x);
// }
// 
// 
// int succ(int x)
// {
//   return x + 1;
// }
// int pred(int x)
// {
//   return x - 1;
// }
// 
// 
// int scl_test_ilist()
// {
//   using namespace doremir::scl;
// // #define xlist(T) ilist<T>
// // #define xlist(T) std::list<T>
// #define xlist(T) std::vector<T>
// //    SCL_CONCEPT_ASSERT(( ForwardContainer< xlist(int) > ));
//   xlist(int) xs;
//   for (int i = 0; i < 10000; ++i)
//     xs = add_before(i, xs);
//   // std::copy( xs.begin(), xs.end(), xs.begin() );
//   std::cout << "\n";
//   std::cout << "Empty : "   << xs.empty() << "\n";
//   std::cout << "Size is : " << xs.size() << "\n";
//   std::cout << "Sum is : "  << std::accumulate(xs.begin(), xs.end(), 0) << "\n";
//   return 0;
// }
