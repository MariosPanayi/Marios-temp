Kilosort_2 - modified files from Zhewei! 
Do not assume they are the same as downloading the repository from Github again!


Before seting up Kilosort:
Need to Install Latest DGPU driver
Install Compatible CUDA [For Matlab 2020a - CUDA 10.1 - update2]
(Check compatibility on this table and ensure compatibility with GPU+latest driver)
https://www.mathworks.com/help/releases/R2021b/parallel-computing/gpu-support-by-release.html
(version for Matlab 2020a)
https://developer.nvidia.com/cuda-10.1-download-archive-update2?target_os=Windows&target_arch=x86_64&target_version=10&target_type=exelocal
Install compatible Visual Studio C++ compiler [Visual STudio 2019 - install the “Desktop Development with C++” workload ]


Mistake made!!!
Need to change everything to be compatible with Matlab 2020b! This is compatible with CUDA 10.2

Note - Initial Setup of CUDA using mexGPUall function - some of the files to be compiled might have access rights errors! This seems to be a problem on NIDA computers due to the CYlance antivirus protection interfering with the reading/writing of some of the .exe files. Solution - unplug the computer from the network when running this.