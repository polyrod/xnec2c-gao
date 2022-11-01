---
title: xnec2c-gao -- A genetic algorithm optimizer for xnec2c
author: Maurizio Di Pietro DC1MDP
date: 1. November 2022
geometry: margin=3cm
---

\newpage
# From .nec to .gao file

A .gao file is essentially a .nec file with special "SYM" and "GSYM" cards.
With those new cardtypes it is possible to genralize your nec model so the optimizer
can generate a whole class of concrete nec models and test them for fitness.

## SYM Card

The SYM Card allows you to bind a expression to a name. After the SYM Card the defined symbol can be
used in arithmetic expressions to calculate antenna parameters. A SYM Card is defined as:

    SYM symbolname := <expr>

where \<expr\> is a mathematical expression consisting of Float/Integer literals,
identifiers and operators +,-,*,/,^,(,),LOG,SIN,COS.

So you could define the speed of light as :

    SYM c := 300000

and your desired frequency as :

    SYM freq := 145800

you can use previosly defined symbols in the rhs of a SYM Card

    SYM vf := 0,96
    SYM lambda := vf * c / freq
    SYM lambdaq := lambda / 4

### Using SYM Card symbols in your GW Cards


To define a dipole you would define a GW Card for the radiator like :

    GW    1   15    -0,49   0   3,92   0,49    0   3,92   0,01    

instead of using concrete values using SYM symbols makes this definition more general:

    SYM height := 2,0 * lambda
    SYM radius := 0,01
    GW    1   15    -lamdaq   0   height   lamdaq    0   height   radius    

It is also possible to use the expression syntax in the GW Cards fields :

    SYM center := 0,0
    SYM height := 2,0 * lambda
    SYM radius := 0,01
    GW    1   15    center-lamdaq   0   height   center+lamdaq    0   height   radius    

By itself SYM Cards give you only the ability to adapt your model easily to different circumstances.
For example to make the 2m-dipole model a dipole for the 10m band, you would only need to change the SYM freq card to reflect
the new 10m band frequency.

    SYM freq := 28480

For xnec2c-gao to be able to optimize this model with an genetic algorithm, we need the ability to define Genes
which are implemented as GSYM Cards.

## GSYM Card

The GSYM Card represents essantialy a gene in the genetic algorithm used to optimize your antenna model.
You can define any number of GSYM Cards each represents a dimension in a multidimensional search space.
Thats why i designed the optimizer as an genetic algorithem , because linear approximation works good
for low dimensional search, whereas genetic algorithem have their strength in multidimensional domains.

The Syntax of a GSYM Card is:

    GSYM symbolname := [<low>...<high>]

where \<low\> and \<high\> are floating point naumbers and low < high.

So to check what radius gives best VSWR/GAIN one would replace the definition of radius from above from:

    SYM radius := 0,01

to

    GSYM radius := [0,001...0,02]

to check radii in the range of 1mm to 2cm.

### Using GSYM Card symbols to "modulate" SYM Card symbols

To vary the parameters defined in terms of SYM cards you can use the GSYM symbols in the SYM definition.

    GSYM  scale := [0,5...1,5]
    SYM   lambda := scale * vf * c / freq

\newpage
# From .gao file to .gao.nec files

After you prepared your .gao file for your antenna model, you can invoke the optimizer.
To get a brief overview how to run the optimizer run:

    $ xnec2c-gao --help

This will yield:
     
    xnec2c-gao - a genetic algorithm optimizer for your antenna model

    Usage: xnec2c-gao (-f|--gaofile FILENAME) [-v] [-d|--select-distinct] 
                      [-s|--population-size INT] [-c|--generation-count INT]

      Run an optimizer for GAOModel FILENAME

    Available options:
      -f,--gaofile FILENAME    GAO Model to optimize
      -v                       How verbose to optimize (can be specified multiple
                               times)
      -d,--select-distinct     Select only distinct individuals as survivors
      -s,--population-size INT How many individuals are in one generation
                               (default: 20)
      -c,--generation-count INT
                               How many generations to run the optimizer
                               (default: 10)
      -h,--help                Show this help text

    Copyright 2022 Maurizio Di Pietro DC1MDP. Program is provided "as is". Author is
    not responsible for any havoc caused by the usage of this software. Use at own
    risk.


## Running your optimization

So to execute your optimization invoke:

    $ xnec2c-gao -d -f dipole.gao

## Fixing errors in gao file

    $ xnec2c-gao -d -f dipole.gao

This will read your .gao file and report any syntactic errors in the .gao file.
With the provided line number and the description what was found and what was expected
it should be, more or less, straigthforward to fix errors.

If for example you defined :

  SYM vk := 0.96

\newpage
and run

    $ xnec2c-gao -f dipole.gao

output would be

    dipole.gao:6:12:
      |
    6 | SYM vk := 0.96
      |            ^^
    unexpected ".9"
    expecting crlf newline, end of input, end of line, or newline
    xnec2c-gao: Abort

This error doesn't indicate that you used the wrong number delimeter , but at least it indicates where to look.

## Output of xnec2c-gao

xnec2c-gao will run for n generations and present you the evolved survivors, you can choose to continue optimizing 
or if you are satisfied quit the optimization in which case xnec2c-gao will write one .nec file for each survivor.
The filename of the necfile will also contain the AVSWR calculated by xnec2c.

## Example output listing
    Generation 1 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 2 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 3 of 20
    <|gbase: 1.1797216|r: 3.914188e-3|scale: 1.1526132|>
    AVG VSWR: 6.11181

    Running Phenotype numeber 4 of 20
    <|gbase: 1.3528808|r: 8.178428e-3|scale: 0.85593957|>
    AVG VSWR: 3.553937

    Running Phenotype numeber 5 of 20
    <|gbase: 3.1454115|r: 9.229292e-3|scale: 1.3438667|>
    AVG VSWR: 10.239396

    Running Phenotype numeber 6 of 20
    <|gbase: 3.767732|r: 7.623251e-3|scale: 1.4082379|>
    AVG VSWR: 13.140715

    Running Phenotype numeber 7 of 20
    <|gbase: 0.74145454|r: 2.814841e-3|scale: 1.2348852|>
    AVG VSWR: 10.802017

    Running Phenotype numeber 8 of 20
    <|gbase: 1.8515602|r: 4.3073315e-3|scale: 1.4454114|>
    AVG VSWR: 17.412685

    Running Phenotype numeber 9 of 20
    <|gbase: 1.7121773|r: 8.892895e-3|scale: 1.064641|>
    AVG VSWR: 3.0502949

    Running Phenotype numeber 10 of 20
    <|gbase: 2.4631903|r: 9.596238e-3|scale: 1.384943|>
    AVG VSWR: 11.250901

    Running Phenotype numeber 11 of 20
    <|gbase: 0.5804702|r: 5.66438e-3|scale: 1.4624045|>
    AVG VSWR: 16.791096

    Running Phenotype numeber 12 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 13 of 20
    <|gbase: 3.413342|r: 7.1589346e-3|scale: 1.3607634|>
    AVG VSWR: 12.018648

    Running Phenotype numeber 14 of 20
    <|gbase: 1.0931046|r: 4.8888233e-3|scale: 1.1403735|>
    AVG VSWR: 5.0878267

    Running Phenotype numeber 15 of 20
    <|gbase: 1.8596292|r: 4.085824e-3|scale: 1.3412089|>
    AVG VSWR: 13.523765

    Running Phenotype numeber 16 of 20
    <|gbase: 0.73438346|r: 2.0980493e-3|scale: 0.8786353|>
    AVG VSWR: 5.792186

    Running Phenotype numeber 17 of 20
    <|gbase: 2.940282|r: 4.8870635e-3|scale: 1.261802|>
    AVG VSWR: 9.655768

    Running Phenotype numeber 18 of 20
    <|gbase: 1.8378383|r: 6.112523e-3|scale: 1.1169642|>
    AVG VSWR: 4.5679517

    Running Phenotype numeber 19 of 20
    <|gbase: 1.6290162|r: 3.3429759e-3|scale: 1.159123|>
    AVG VSWR: 6.1940093

    Running Phenotype numeber 20 of 20
    <|gbase: 2.8688822|r: 6.1955117e-3|scale: 0.7232177|>
    AVG VSWR: 21.100103
    Just 1.510018
    Just 1.510018
    Just 1.510018
    Just 1.510018
    Just 1.5149671
    Just 3.0502949
    Just 3.553937
    Just 4.5679517
    Just 5.0878267
    Generation 2 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 2 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 3 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 4 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 5 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 6 of 20
    <|gbase: 1.7121773|r: 8.892895e-3|scale: 1.064641|>
    AVG VSWR: 3.0502949

    Running Phenotype numeber 7 of 20
    <|gbase: 1.3528808|r: 8.178428e-3|scale: 0.85593957|>
    AVG VSWR: 3.553937

    Running Phenotype numeber 8 of 20
    <|gbase: 1.8378383|r: 6.112523e-3|scale: 1.1169642|>
    AVG VSWR: 4.5679517

    Running Phenotype numeber 9 of 20
    <|gbase: 1.0931046|r: 4.8888233e-3|scale: 1.1403735|>
    AVG VSWR: 5.0878267

    Running Phenotype numeber 10 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.88905215|>
    AVG VSWR: 2.2887287

    Running Phenotype numeber 11 of 20
    <|gbase: 3.0134964|r: 8.0706984e-2|scale: 0.9318137|>
    AVG VSWR: 3.042784

    Running Phenotype numeber 12 of 20
    <|gbase: 2.8181305|r: 2.5222521e-2|scale: 1.3061999|>
    AVG VSWR: 6.979472

    Running Phenotype numeber 13 of 20
    <|gbase: 2.700207|r: 2.0798415e-2|scale: 1.3061999|>
    AVG VSWR: 7.2308474

    Running Phenotype numeber 14 of 20
    <|gbase: 0.81034976|r: 5.470655e-3|scale: 0.96317196|>
    AVG VSWR: 1.5298344

    Running Phenotype numeber 15 of 20
    <|gbase: 1.6574975|r: 8.71753e-2|scale: 1.020966|>
    AVG VSWR: 3.6696475

    Running Phenotype numeber 16 of 20
    <|gbase: 1.4324692|r: 3.4421407e-2|scale: 0.85593957|>
    AVG VSWR: 1.6776508

    Running Phenotype numeber 17 of 20
    <|gbase: 1.901175|r: 1.3902565e-3|scale: 1.0374831|>
    AVG VSWR: 2.265661

    Running Phenotype numeber 18 of 20
    <|gbase: 1.1171545|r: 3.3019066e-2|scale: 1.0770596|>
    AVG VSWR: 2.9681983

    Running Phenotype numeber 19 of 20
    <|gbase: 3.3357058|r: 7.994094e-3|scale: 1.1204023|>
    AVG VSWR: 4.4607906

    Running Phenotype numeber 20 of 20
    <|gbase: 0.5826938|r: 8.8746045e-3|scale: 1.2239335|>
    AVG VSWR: 7.096812
    Just 1.510018
    Just 1.510018
    Just 1.510018
    Just 1.510018
    Just 1.5149671
    Just 1.5298344
    Just 1.6776508
    Just 2.265661
    Just 2.2887287
    Generation 3 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 2 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 3 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 4 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 5 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 6 of 20
    <|gbase: 0.81034976|r: 5.470655e-3|scale: 0.96317196|>
    AVG VSWR: 1.5298344

    Running Phenotype numeber 7 of 20
    <|gbase: 1.4324692|r: 3.4421407e-2|scale: 0.85593957|>
    AVG VSWR: 1.6776508

    Running Phenotype numeber 8 of 20
    <|gbase: 1.901175|r: 1.3902565e-3|scale: 1.0374831|>
    AVG VSWR: 2.265661

    Running Phenotype numeber 9 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.88905215|>
    AVG VSWR: 2.2887287

    Running Phenotype numeber 10 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 11 of 20
    <|gbase: 3.110341|r: 9.4134666e-2|scale: 0.9393666|>
    AVG VSWR: 5.2879887

    Running Phenotype numeber 12 of 20
    <|gbase: 2.8008175|r: 8.4065095e-2|scale: 1.3061999|>
    AVG VSWR: 5.1953034

    Running Phenotype numeber 13 of 20
    <|gbase: 2.8693385|r: 1.1356957e-2|scale: 1.2064745|>
    AVG VSWR: 6.437683

    Running Phenotype numeber 14 of 20
    <|gbase: 0.770557|r: 9.116748e-2|scale: 0.9457245|>
    AVG VSWR: 4.7304864

    Running Phenotype numeber 15 of 20
    <|gbase: 0.8211207|r: 5.470655e-3|scale: 0.96317196|>
    AVG VSWR: 1.5435065

    Running Phenotype numeber 16 of 20
    <|gbase: 1.4870085|r: 2.655287e-2|scale: 0.85593957|>
    AVG VSWR: 1.8651153

    Running Phenotype numeber 17 of 20
    <|gbase: 1.9675366|r: 1.3335128e-3|scale: 1.0931814|>
    AVG VSWR: 4.084516

    Running Phenotype numeber 18 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5178698

    Running Phenotype numeber 19 of 20
    <|gbase: 3.5430398|r: 2.0837623e-3|scale: 1.1745347|>
    AVG VSWR: 7.415517

    Running Phenotype numeber 20 of 20
    <|gbase: 2.4624627|r: 4.6071922e-3|scale: 1.0097835|>
    AVG VSWR: 1.8614281
    Just 1.4941572
    Just 1.4941572
    Just 1.510018
    Just 1.510018
    Just 1.510018
    Just 1.5149671
    Just 1.5178698
    Just 1.5298344
    Just 1.5435065
    Generation 4 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 2 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 3 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 4 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 5 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 6 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 7 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5178698

    Running Phenotype numeber 8 of 20
    <|gbase: 0.81034976|r: 5.470655e-3|scale: 0.96317196|>
    AVG VSWR: 1.5298344

    Running Phenotype numeber 9 of 20
    <|gbase: 0.8211207|r: 5.470655e-3|scale: 0.96317196|>
    AVG VSWR: 1.5435065

    Running Phenotype numeber 10 of 20
    <|gbase: 2.920154|r: 6.6324264e-2|scale: 0.9544021|>
    AVG VSWR: 2.313508

    Running Phenotype numeber 11 of 20
    <|gbase: 3.0001845|r: 7.8051865e-2|scale: 0.98123175|>
    AVG VSWR: 2.9829473

    Running Phenotype numeber 12 of 20
    <|gbase: 3.0134964|r: 7.089965e-2|scale: 1.0439494|>
    AVG VSWR: 3.01104

    Running Phenotype numeber 13 of 20
    <|gbase: 2.925304|r: 7.470051e-2|scale: 0.9373774|>
    AVG VSWR: 2.5967855

    Running Phenotype numeber 14 of 20
    <|gbase: 2.6855443|r: 4.9670633e-2|scale: 1.3061999|>
    AVG VSWR: 5.2933264

    Running Phenotype numeber 15 of 20
    <|gbase: 0.86154795|r: 5.673089e-3|scale: 1.0404276|>
    AVG VSWR: 2.5793397

    Running Phenotype numeber 16 of 20
    <|gbase: 3.0544581|r: 3.9832544e-2|scale: 0.9439138|>
    AVG VSWR: 1.7883464

    Running Phenotype numeber 17 of 20
    <|gbase: 0.8614461|r: 7.508151e-2|scale: 0.9054364|>
    AVG VSWR: 2.7019482

    Running Phenotype numeber 18 of 20
    <|gbase: 0.7627335|r: 3.4439296e-2|scale: 0.96317196|>
    AVG VSWR: 2.0128226

    Running Phenotype numeber 19 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 20 of 20
    <|gbase: 2.8696976|r: 8.264191e-3|scale: 1.4803185|>
    AVG VSWR: 14.927701
    Just 1.4941572
    Just 1.4941572
    Just 1.5089303
    Just 1.5089303
    Just 1.510018
    Just 1.510018
    Just 1.5149671
    Just 1.5178698
    Just 1.5298344
    Generation 5 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 2 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 3 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 4 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 5 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 6 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 7 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 8 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5178698

    Running Phenotype numeber 9 of 20
    <|gbase: 0.81034976|r: 5.470655e-3|scale: 0.96317196|>
    AVG VSWR: 1.5298344

    Running Phenotype numeber 10 of 20
    <|gbase: 2.9508934|r: 6.607434e-2|scale: 0.98464376|>
    AVG VSWR: 2.4988732

    Running Phenotype numeber 11 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.8897629|>
    AVG VSWR: 2.196433

    Running Phenotype numeber 12 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.8743641|>
    AVG VSWR: 3.271197

    Running Phenotype numeber 13 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 14 of 20
    <|gbase: 3.034425|r: 6.3404486e-2|scale: 0.8690407|>
    AVG VSWR: 1.9396722

    Running Phenotype numeber 15 of 20
    <|gbase: 2.7838097|r: 3.224796e-2|scale: 1.3719617|>
    AVG VSWR: 7.2617536

    Running Phenotype numeber 16 of 20
    <|gbase: 0.87687993|r: 5.673089e-3|scale: 1.004675|>
    AVG VSWR: 1.9138815

    Running Phenotype numeber 17 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5178698

    Running Phenotype numeber 18 of 20
    <|gbase: 0.81034976|r: 2.9754687e-2|scale: 1.0158169|>
    AVG VSWR: 2.5754268

    Running Phenotype numeber 19 of 20
    <|gbase: 2.5915706|r: 2.9640447e-3|scale: 0.7248825|>
    AVG VSWR: 34.30213

    Running Phenotype numeber 20 of 20
    <|gbase: 3.363192|r: 3.5185628e-3|scale: 1.4683347|>
    AVG VSWR: 19.615362
    Just 1.4740518
    Just 1.4740518
    Just 1.4941572
    Just 1.4941572
    Just 1.5089303
    Just 1.510018
    Just 1.510018
    Just 1.5149671
    Just 1.5178698
    Generation 6 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 2 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 3 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 4 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 5 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 6 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 7 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 8 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 9 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5178698

    Running Phenotype numeber 10 of 20
    <|gbase: 1.2784703|r: 9.113649e-2|scale: 0.875676|>
    AVG VSWR: 5.115861

    Running Phenotype numeber 11 of 20
    <|gbase: 1.2646182|r: 1.0374843e-2|scale: 0.99215335|>
    AVG VSWR: 1.8440496

    Running Phenotype numeber 12 of 20
    <|gbase: 2.920154|r: 2.6499622e-2|scale: 0.9901486|>
    AVG VSWR: 2.0931616

    Running Phenotype numeber 13 of 20
    <|gbase: 2.826513|r: 2.8303757e-2|scale: 0.9544021|>
    AVG VSWR: 1.7288566

    Running Phenotype numeber 14 of 20
    <|gbase: 1.2617242|r: 1.3629324e-2|scale: 0.9606955|>
    AVG VSWR: 1.5587474

    Running Phenotype numeber 15 of 20
    <|gbase: 3.0134964|r: 8.60088e-2|scale: 0.9544021|>
    AVG VSWR: 3.694283

    Running Phenotype numeber 16 of 20
    <|gbase: 2.7838097|r: 5.79921e-2|scale: 1.2443559|>
    AVG VSWR: 4.6548443

    Running Phenotype numeber 17 of 20
    <|gbase: 0.7505821|r: 1.054535e-2|scale: 1.0324318|>
    AVG VSWR: 2.6425264

    Running Phenotype numeber 18 of 20
    <|gbase: 3.0140324|r: 5.7466343e-2|scale: 0.9439138|>
    AVG VSWR: 2.0377064

    Running Phenotype numeber 19 of 20
    <|gbase: 3.3891652|r: 4.218396e-3|scale: 1.3784698|>
    AVG VSWR: 14.935855

    Running Phenotype numeber 20 of 20
    <|gbase: 1.5580232|r: 2.8773393e-3|scale: 0.73912525|>
    AVG VSWR: 30.261736
    Just 1.4740518
    Just 1.4740518
    Just 1.4941572
    Just 1.4941572
    Just 1.5089303
    Just 1.510018
    Just 1.510018
    Just 1.5149671
    Just 1.5178698
    Generation 7 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 2 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 3 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 4 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 5 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 6 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 7 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 8 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 9 of 20
    <|gbase: 3.0544581|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5178698

    Running Phenotype numeber 10 of 20
    <|gbase: 1.2698841|r: 0.10228565|scale: 0.91130465|>
    AVG VSWR: 9.552692

    Running Phenotype numeber 11 of 20
    <|gbase: 1.3086301|r: 6.1218953e-3|scale: 0.9330137|>
    AVG VSWR: 1.7019237

    Running Phenotype numeber 12 of 20
    <|gbase: 3.011912|r: 8.584733e-2|scale: 1.0151532|>
    AVG VSWR: 3.7634575

    Running Phenotype numeber 13 of 20
    <|gbase: 2.920154|r: 1.6171772e-2|scale: 0.91551125|>
    AVG VSWR: 1.5129

    Running Phenotype numeber 14 of 20
    <|gbase: 1.187158|r: 1.7550275e-2|scale: 0.89992934|>
    AVG VSWR: 1.525112

    Running Phenotype numeber 15 of 20
    <|gbase: 3.0134964|r: 6.553517e-2|scale: 0.90006053|>
    AVG VSWR: 2.0557096

    Running Phenotype numeber 16 of 20
    <|gbase: 2.7838097|r: 3.0447572e-2|scale: 1.3204651|>
    AVG VSWR: 6.7044554

    Running Phenotype numeber 17 of 20
    <|gbase: 0.79892266|r: 1.1608964e-3|scale: 0.96317196|>
    AVG VSWR: 1.9070727

    Running Phenotype numeber 18 of 20
    <|gbase: 3.0610757|r: 9.993009e-3|scale: 0.9439138|>
    AVG VSWR: 1.5172143

    Running Phenotype numeber 19 of 20
    <|gbase: 1.4993876|r: 4.087153e-3|scale: 1.4750845|>
    AVG VSWR: 17.54432

    Running Phenotype numeber 20 of 20
    <|gbase: 3.0438979|r: 3.1955e-3|scale: 1.0557034|>
    AVG VSWR: 2.7315946
    Just 1.4740518
    Just 1.4740518
    Just 1.4941572
    Just 1.4941572
    Just 1.5089303
    Just 1.510018
    Just 1.510018
    Just 1.5129
    Just 1.5149671
    Generation 8 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 2 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 3 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 4 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 5 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 6 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 7 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 8 of 20
    <|gbase: 2.920154|r: 1.6171772e-2|scale: 0.91551125|>
    AVG VSWR: 1.5129

    Running Phenotype numeber 9 of 20
    <|gbase: 0.79892266|r: 5.673089e-3|scale: 0.96317196|>
    AVG VSWR: 1.5149671

    Running Phenotype numeber 10 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.8975656|>
    AVG VSWR: 1.926402

    Running Phenotype numeber 11 of 20
    <|gbase: 1.2330761|r: 1.0374843e-2|scale: 0.95601714|>
    AVG VSWR: 1.4296304

    Running Phenotype numeber 12 of 20
    <|gbase: 2.920154|r: 1.5692469e-2|scale: 0.9035439|>
    AVG VSWR: 1.6064498

    Running Phenotype numeber 13 of 20
    <|gbase: 2.8614676|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4700874

    Running Phenotype numeber 14 of 20
    <|gbase: 1.3489581|r: 5.145952e-2|scale: 0.93844926|>
    AVG VSWR: 1.9739485

    Running Phenotype numeber 15 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9273327|>
    AVG VSWR: 1.624727

    Running Phenotype numeber 16 of 20
    <|gbase: 2.7838097|r: 8.172559e-2|scale: 1.2568328|>
    AVG VSWR: 4.7986703

    Running Phenotype numeber 17 of 20
    <|gbase: 2.9728408|r: 2.9758621e-2|scale: 0.8879158|>
    AVG VSWR: 1.5239573

    Running Phenotype numeber 18 of 20
    <|gbase: 0.8965912|r: 7.870366e-2|scale: 1.050587|>
    AVG VSWR: 3.5160549

    Running Phenotype numeber 19 of 20
    <|gbase: 3.2585723|r: 4.5694127e-3|scale: 0.824189|>
    AVG VSWR: 8.34432

    Running Phenotype numeber 20 of 20
    <|gbase: 1.8740841|r: 7.882059e-3|scale: 1.4180793|>
    AVG VSWR: 13.293427
    Just 1.4296304
    Just 1.4296304
    Just 1.4700874
    Just 1.4700874
    Just 1.4740518
    Just 1.4941572
    Just 1.5089303
    Just 1.510018
    Just 1.510018
    Generation 9 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 1.2330761|r: 1.0374843e-2|scale: 0.95601714|>
    AVG VSWR: 1.4296304

    Running Phenotype numeber 2 of 20
    <|gbase: 1.2330761|r: 1.0374843e-2|scale: 0.95601714|>
    AVG VSWR: 1.4296304

    Running Phenotype numeber 3 of 20
    <|gbase: 2.8614676|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4700874

    Running Phenotype numeber 4 of 20
    <|gbase: 2.8614676|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4700874

    Running Phenotype numeber 5 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 6 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 7 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 8 of 20
    <|gbase: 3.0134964|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 9 of 20
    <|gbase: 2.7838097|r: 6.4669326e-3|scale: 1.3061999|>
    AVG VSWR: 1.510018

    Running Phenotype numeber 10 of 20
    <|gbase: 1.2330761|r: 6.988276e-2|scale: 0.9440006|>
    AVG VSWR: 2.2948909

    Running Phenotype numeber 11 of 20
    <|gbase: 1.2330761|r: 2.2693075e-2|scale: 0.95601714|>
    AVG VSWR: 1.6548808

    Running Phenotype numeber 12 of 20
    <|gbase: 2.7840083|r: 4.2018324e-2|scale: 0.9544021|>
    AVG VSWR: 1.8608981

    Running Phenotype numeber 13 of 20
    <|gbase: 2.901591|r: 6.293836e-2|scale: 0.91906613|>
    AVG VSWR: 2.0237882

    Running Phenotype numeber 14 of 20
    <|gbase: 1.3341603|r: 1.0329439e-2|scale: 0.9432163|>
    AVG VSWR: 1.4939926

    Running Phenotype numeber 15 of 20
    <|gbase: 2.9599693|r: 0.102004394|scale: 0.9544021|>
    AVG VSWR: 8.286179

    Running Phenotype numeber 16 of 20
    <|gbase: 1.2581857|r: 2.5936756e-2|scale: 0.9432163|>
    AVG VSWR: 1.6195178

    Running Phenotype numeber 17 of 20
    <|gbase: 3.080188|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4982018

    Running Phenotype numeber 18 of 20
    <|gbase: 2.7113428|r: 3.1715542e-2|scale: 1.364424|>
    AVG VSWR: 7.0448046

    Running Phenotype numeber 19 of 20
    <|gbase: 2.7023444|r: 5.0837765e-3|scale: 1.3840201|>
    AVG VSWR: 14.156875

    Running Phenotype numeber 20 of 20
    <|gbase: 3.4925072|r: 7.3423744e-3|scale: 1.0520517|>
    AVG VSWR: 2.762753
    Just 1.4296304
    Just 1.4296304
    Just 1.4700874
    Just 1.4700874
    Just 1.4740518
    Just 1.4939926
    Just 1.4941572
    Just 1.4982018
    Just 1.5089303
    Generation 10 of 10

    Running Phenotype numeber 1 of 20
    <|gbase: 1.2330761|r: 1.0374843e-2|scale: 0.95601714|>
    AVG VSWR: 1.4296304

    Running Phenotype numeber 2 of 20
    <|gbase: 1.2330761|r: 1.0374843e-2|scale: 0.95601714|>
    AVG VSWR: 1.4296304

    Running Phenotype numeber 3 of 20
    <|gbase: 2.8614676|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4700874

    Running Phenotype numeber 4 of 20
    <|gbase: 2.8614676|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4700874

    Running Phenotype numeber 5 of 20
    <|gbase: 1.3128989|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.4740518

    Running Phenotype numeber 6 of 20
    <|gbase: 1.3341603|r: 1.0329439e-2|scale: 0.9432163|>
    AVG VSWR: 1.4939926

    Running Phenotype numeber 7 of 20
    <|gbase: 2.920154|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4941572

    Running Phenotype numeber 8 of 20
    <|gbase: 3.080188|r: 9.993009e-3|scale: 0.9544021|>
    AVG VSWR: 1.4982018

    Running Phenotype numeber 9 of 20
    <|gbase: 1.2581857|r: 6.660261e-3|scale: 0.9432163|>
    AVG VSWR: 1.5089303

    Running Phenotype numeber 10 of 20
    <|gbase: 1.2608074|r: 1.0374843e-2|scale: 0.9482386|>
    AVG VSWR: 1.4287896

    Running Phenotype numeber 11 of 20
    <|gbase: 1.2820209|r: 1.0374843e-2|scale: 0.95601714|>
    AVG VSWR: 1.4785011

    Running Phenotype numeber 12 of 20
    <|gbase: 2.9207225|r: 5.0143003e-2|scale: 0.9544021|>
    AVG VSWR: 2.0124831

    Running Phenotype numeber 13 of 20
    <|gbase: 2.8614676|r: 2.3571327e-3|scale: 0.9758084|>
    AVG VSWR: 1.5194361

    Running Phenotype numeber 14 of 20
    <|gbase: 1.3439138|r: 1.0374843e-2|scale: 0.9432163|>
    AVG VSWR: 1.5024515

    Running Phenotype numeber 15 of 20
    <|gbase: 1.3341603|r: 5.0170578e-2|scale: 0.9432163|>
    AVG VSWR: 1.9847287

    Running Phenotype numeber 16 of 20
    <|gbase: 2.920154|r: 5.152002e-3|scale: 0.9544021|>
    AVG VSWR: 1.5595306

    Running Phenotype numeber 17 of 20
    <|gbase: 3.090234|r: 4.8895672e-2|scale: 0.868411|>
    AVG VSWR: 1.6293423

    Running Phenotype numeber 18 of 20
    <|gbase: 1.2529013|r: 6.6879004e-2|scale: 0.9432163|>
    AVG VSWR: 2.2214837

    Running Phenotype numeber 19 of 20
    <|gbase: 1.1275392|r: 5.800803e-3|scale: 1.093349|>
    AVG VSWR: 3.6882498

    Running Phenotype numeber 20 of 20
    <|gbase: 2.7625496|r: 9.63055e-3|scale: 1.4622915|>
    AVG VSWR: 13.702303
    Just 1.4287896
    Just 1.4287896
    Just 1.4296304
    Just 1.4296304
    Just 1.4700874
    Just 1.4740518
    Just 1.4785011
    Just 1.4939926
    Just 1.4941572
    Are you satisfied and want to [Q]uit or [P]roceed for another 10 generations ?
