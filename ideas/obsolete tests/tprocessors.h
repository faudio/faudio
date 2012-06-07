/*
 *  core.h
 *  sclaudio
 *
 *  Created by Hans Höglund on 2011-11-03.
 *  Copyright 2011 DoReMIR http://www.doremir.com/. All rights reserved.
 *
 */


//class Hans : public AudioProcessor 
//{
//public:
//    declareName("Hans");
//    
//    declareInputs(5); 
//    declareOutputs(2);
//    
//    declareControls 
//    {
//        declareControl(Compose);
//        declareControl(Program);
//    };
//    declareNumberOfControls(2);
//    
//    void prepare(Signal &signal, Control &control){}
//    void process(Signal &signal, Control &control) const{}
//    void cleanup(Signal &signal, Control &control){}
//};
// 
//class Youness : public AudioProcessor 
//{
//public:
//    declareName("Youness");
//    
//    declareInputs(2); 
//    declareOutputs(5);
//    
//    declareControls 
//    {
//        declareControl(Compose);
//        declareControl(Fight);
//        declareControl(Rap);
//    };
//    declareNumberOfControls(3);
//    
//    void prepare(Signal &signal, Control &control){}
//    void process(Signal &signal, Control &control) const{}
//    void cleanup(Signal &signal, Control &control){}
//};
//
//
//void testDeclareControls()
//{
//    std::cout << "========\n";
//    std::cout << "Testing processor control declarations\n";
//
//    Hans h;
//    Youness y;
//    AudioProcessor* s = AudioProcessor::sequence(listFrom<AudioProcessor*>(&h, &y));
//    AudioProcessor* p = AudioProcessor::parallel(listFrom<AudioProcessor*>(&h, &y));
//
//    cout << h.name() << "\n";
//    cout << y.name() << "\n";
//
//    cout << p->name() << "\n";
//
//    vector<string> parControls;
//    p->controls(parControls);
//    printAll(parControls);
//    cout << "\n";
//    
//
//    cout << s->name() << "\n";
//
//    vector<string> seqControls;
//    s->controls(seqControls);
//    printAll(seqControls);
//    cout << "\n";
//
//    cout << "Offsets:\n";
//    cout << s->getControlOffset(h) << "\n";
//    cout << s->getControlOffset(y) << "\n";
//    cout << "\n";
//
//    cout << "Indices:\n";
//    cout << s->getControlIndex(h, "Program") << "\n";
//    cout << s->getControlIndex(y, "Rap") << "\n";
//    cout << s->getControlIndex(h, "Compose") << "\n";
//    cout << s->getControlIndex(y, "Compose") << "\n";
//    cout << s->getControlIndex(y, "Undef") << "\n";
//    cout << "\n";
//
//
//    cout << "\n";
//
//}