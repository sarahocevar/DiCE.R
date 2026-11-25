# DiCE.R
Decentralised Distributed Computing Environment for R (DiCE.R) is designed to simplify the distribution of computational jobs across multiple CPUs and computers using shared network storage.

## Description
DiCE.R is a solution for anyone who wishes to skip the programmatic implementation steps for running computations on multiple threads and/or to process the set of computational jobs utilizing multiple independent computing units with only a shared memory storage and without the need of installing additional third-party software. DiCE.R can be used to distribute computational jobs across a swarm of autonomous workers that share the execution of the jobs. DiCE.R also enables automated generation of multi-factorial experiments with full-factorial design. Full-factorial design means that the parameter space of all possible combinations of the predefined parameter levels is systematically investigated in simulations using each possible combination of the parameters. Another contrasting use case for DiCE.R is Monte Carlo simulations, which utilize random parameter value combinations by sampling them from a given joint probability distribution instead of exploring a predefined discrete parameter space. DiCE.R greatly reduces the time and the amount of manual work needed for job allocation in large-scale computations utilizing distributed computing across multiple computing systems. It also tracks the completed jobs and allows the user to easily rerun the jobs that failed.

## Installation & Usage
For installation and usage refer to Docs/DiCER_User_Guide.pdf

## Support
For support please contact sara.s.hocevar@jyu.fi

## Authors and acknowledgment
Tommi Perälä, Sara Hocevar, Tiina Sävilammi, Jaakko Litmanen,  Anna Kuparinen

We thank Sami Lehtinen for testing the software. This study was funded by the Academy of Finland (project grant 317495 to A.K.), by the Nordforsk (consortium grant IMP-FISH no. 195538a), and the European Research Council (COMPLEX-FISH 770884 to A.K.). The present study reflects only the authors’ view; the European Research Council is not responsible for any use that may be made of the information it contains.

## License
See LICENSE or Apache-2.0 license for details 
