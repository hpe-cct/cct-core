# cct-core

The core library of the HPE Cognitive Computing Toolkit (CCT) is a platform enabling simple
programming of GPUs that support OpenCL.  The library offers a simple abstraction for
structured data (a `Field`) and a means for combining fields into a stateful function (a
`ComputeGraph`).  Users describe their functions via the execution of a Scala program,
and can then continue to manipulate the resulting `ComputeGraph` programatically or through
an intuitive GUI.  While the platform offers many general-purpose operators for transforming
`Fields`, the design focus has been to provide operators needed by Convolutional Neural Nets.

The cct-core is currently supported under the following 64-bit OS's: RHEL7, Ubuntu 14.04,
Windows7 and Windows8.  Also, GPU hardware from NVIDIA (Kepler architecture or later) or
AMD (Firepro) is supported.

If you're new to CCT, start with the [tutorial](https://github.com/hpe-cct/cct-tutorial).
