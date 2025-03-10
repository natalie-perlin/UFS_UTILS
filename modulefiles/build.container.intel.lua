help([[
Load environment to compile UFS_UTILS in a container using Intel
]])

prepend_path("MODULEPATH", "/opt/spack-stack/spack-stack-1.8.0/envs/unified-env/install/modulefiles/Core")

stack_intel_ver=os.getenv("stack_intel_ver") or "2021.10.0"
load(pathJoin("stack-intel", stack_intel_ver))

stack_impi_ver=os.getenv("stack_impi_ver") or "2021.12.1"
load(pathJoin("stack-intel-oneapi-mpi", stack_impi_ver))

cmake_ver=os.getenv("cmake_ver") or "3.27.9"
load(pathJoin("cmake", cmake_ver))

bacio_ver=os.getenv("bacio_ver") or "2.4.1"
load(pathJoin("bacio", bacio_ver))

g2_ver=os.getenv("g2_ver") or "3.5.1"
load(pathJoin("g2", g2_ver))

ip_ver=os.getenv("ip_ver") or "5.0.0"
load(pathJoin("ip", ip_ver))

nemsio_ver=os.getenv("nemsio_ver") or "2.5.4"
load(pathJoin("nemsio", nemsio_ver))

sp_ver=os.getenv("sp_ver") or "2.5.0"
load(pathJoin("sp", sp_ver))

w3emc_ver=os.getenv("w3emc_ver") or "2.10.0"
load(pathJoin("w3emc", w3emc_ver))

sigio_ver=os.getenv("sigio_ver") or "2.3.2"
load(pathJoin("sigio", sigio_ver))

png_ver=os.getenv("png_ver") or "1.6.37"
load(pathJoin("libpng", png_ver))

netcdf_c_ver=os.getenv("netcdf_c_ver") or "4.9.2"
load(pathJoin("netcdf-c", netcdf_c_ver))

netcdf_fortran_ver=os.getenv("netcdf_fortran_ver") or "4.6.1"
load(pathJoin("netcdf-fortran", netcdf_fortran_ver))

nccmp_ver=os.getenv("nccmp_ver") or "1.9.0.1"
load(pathJoin("nccmp", nccmp_ver))

esmf_ver=os.getenv("esmf_ver") or "8.6.1"
load(pathJoin("esmf", esmf_ver))

nco_ver=os.getenv("nco_ver") or "5.1.6"
load(pathJoin("nco", nco_ver))

whatis("Description: UFS_UTILS build environment")
