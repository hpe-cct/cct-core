/*
 * (c) Copyright 2016 Hewlett Packard Enterprise Development LP
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package cogjni;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.util.Arrays;

/**
 * Created by karenbre on 10/27/2015.
 */
/**
 * Wrapper class for JNI interface to C native cog runtime
 *
 * Methods are pattered after the C cogFunctionInterface
 * All methods will throw a java.lang.RuntimeException on any cog error
 * Note: a null pointer access violation on the native side will crash the JVM as that is not a C++ exception
 * that can be caught on the native side.
 * Any uncaught C++ exception will also crash the JVM
 *
 * To generate the JNI C header file from this class to be used in the cognative build, first compile this class,
 * then use javah (in your <jdk_home>/bin):
 *   cd  <libcog directory>/target/scala-2.10/classes
 *   javah -jni -d <cognative source directory>/CogRuntimeJNI -classpath . -stubs cogjni.CogJNIWrapper
 *
 * To run the main method of this class you need to copy CogRuntimeJNI.dll and hdf5.dll
 *   into <libcog directory>\target\scala-2.10\classes\cogjni
 * In your IDEA Run configurations
 *    set VM Options to -Djava.library.path=.
 *    set working directory to <libcog directory>\target\scala-2.10\classes\cogjni
 */


public class CogJNIWrapper {
    static {
        // load library
        System.loadLibrary("hdf5");
        System.loadLibrary("CogRuntimeJNI");
    }

    // CogElementType
    static int COG_FLOAT16 = 0;
    static int COG_FLOAT32 = 1;
    static int COG_FLOAT64 = 2;
    static int COG_INT8 = 3;
    static int COG_INT16 = 4;
    static int COG_INT32 = 5;
    static int COG_INT64 = 6;
    static int COG_UINT8 = 7;
    static int COG_UINT16 = 8;
    static int COG_UINT32 = 9;
    static int COG_UINT64 = 10;
    static int COG_UINT8PIXEL = 11;
    static int COG_COMPLEX16 = 12;
    static int COG_COMPLEX32 = 13;
    static int COG_COMPLEX64 = 14;

    /** opaque pointer to the native cog ComputeFunction */
    private long ComputeFunctionPtr;

    /** The major version number of the native runtime. */
    public native int cogFunctionMajorVersionNumber();

    /** The minor version number of the native runtime. */
    public native int cogFunctionMinorVersionNumber();

    /** The number of OpenCL devices (not necessarily GPUs) on the OpenCL platform that will be used. */
    public native int cogFunctionAvailableGPUs();

    /** Create a CogComputeFunction from the descriptions read from the HDF5 file `computeGraphFilename`. */
    public native long cogFunctionCreateComputeFunction(String computeGraphFilename, int gpuIndex);

    /* Function Interface */

    /** The number of independent threads needed to get the maximum throughput. */
    public native int cogFunctionOptimalThreadCount(long computeFunction);

    /** The number of the inputs to the CogComputeFunction (as determined by the sensors in the CogComputeGraph). */
    public native int cogFunctionInputCount(long computeFunction);

    /** The name of the `inputIndex` input to the CogComputeFunction (as determined by the sensors in the CogComputeGraph). */
    public native String cogFunctionInputName(long computeFunction, int inputIndex);

    /** The FieldType of the `inputIndex` input to the CogComputeFunction (as determined by the sensors in the CogComputeGraph). */
    public native long cogFunctionInputFieldType(long computeFunction, int inputIndex);

    /** The number of the outputs to the ComputeFunction (as determined by the actuators in the CogComputeGraph). */
    public native int cogFunctionOutputCount(long computeFunction);

    /** The name of the `outputIndex` output to the ComputeFunction (as determined by the actuators in the CogComputeGraph). */
    public native String cogFunctionOutputName(long computeFunction, int outputIndex);

    /** The FieldType of the `outputIndex` output to the CogComputeFunction (as determined by the actuators in the CogComputeGraph). */
    public native long cogFunctionOutputFieldType(long computeFunction, int outputIndex);

    /** Execute the function embodied by the ComputeFunction, reading the inputBuffers and writing the outputBuffers. */
    // use input and output buffers end-to-end on a 1D array - first try
    public native void cogFunctionEvaluate(long computeFunction,  byte[] inputBuffers,  byte[] outputBuffers);
    // use input and output buffers as 2D arrays - we should probably choose this way to match C function interface
    // and minimize potential for array indexing errors
    public native void cogFunctionEvaluate2D(long computeFunction,  byte[][] inputBuffers,  byte[][] outputBuffers);

    /** The name of the ComputeGraph wrapped by the ComputeFunction. */
    public native String cogFunctionName(long computeFunction);

    /** read the data out of the field identified by name into the fieldBuffer **/
    public native void cogReadFieldByName(long computeFunction, String fieldName, float fieldBuffer[]);

    /** get the field type of the tensor with name 'fieldName' **/
    public native long cogTensorFieldType(long computeFunction, String fieldName);

    /* Termination */

    /** Free the resources associated with `computeFunction`. */
    public native int cogFunctionShutdown(long computeFunction);

    /** Free the resources associated with all previously instantiated CogComputeFunctions. */
    public native int cogFunctionShutdownAll();

    /* FieldType exploration */

    /** The field dimensions of the field described by `fieldType`. */
    public native int cogFieldTypeDimensions( long fieldType);

    /** The tensor order (i.e. dimensions) of the field described by `fieldType`. */
    public native int cogFieldTypeTensorOrder( long fieldType);

    /** The size of the 3rd field dimension of the field described by `fieldType`. */
    public native int cogFieldTypeLayers( long fieldType);

    /** The size of the 2nd field dimension of the field described by `fieldType`. */
    public native int cogFieldTypeRows(long fieldType);

    /** The size of the 1st field dimension of the field described by `fieldType`. */
    public native int cogFieldTypeColumns(long fieldType);

    /** The size of the 2nd tensor dimension of the field described by `fieldType`. */
    public native int cogFieldTypeTensorRows( long fieldType);

    /** The size of the 1st tensor dimension of the field described by `fieldType`. */
    public native int cogFieldTypeTensorColumns( long fieldType);

    /** The size of the buffer in bytes of the field described by `fieldType`. */
    public native int cogFieldTypeBufferSizeBytes(long fieldType);

    /** The types of elements in the tensors of the field described by `fieldType`. */
    public native int cogFieldTypeElementType( long fieldType);


    // test
    public static void main(final String[] args) {
        CogJNIWrapper wrapper = new CogJNIWrapper();
        System.out.format("CogRuntime Major version: %d\n", wrapper.cogFunctionMajorVersionNumber());
        System.out.format("CogRuntime Minor version: %d\n", wrapper.cogFunctionMinorVersionNumber());
        System.out.format("available GPUs: %d\n", wrapper.cogFunctionAvailableGPUs());

        System.out.format("***Loading file incrementer.h5.....\n");
        wrapper.ComputeFunctionPtr = wrapper.cogFunctionCreateComputeFunction("incrementer.h5", 0);
        String name = wrapper.cogFunctionName(wrapper.ComputeFunctionPtr);
        System.out.format("Computegraph name: %s\n", name);
        int threadCount = wrapper.cogFunctionOptimalThreadCount(wrapper.ComputeFunctionPtr);
        System.out.format("thread count: %d\n", threadCount);

        // check inputs and outputs
        int inputs = wrapper.cogFunctionInputCount(wrapper.ComputeFunctionPtr);
        System.out.format("input count: %d\n", inputs);
        String inputName = wrapper.cogFunctionInputName(wrapper.ComputeFunctionPtr, 0);
        System.out.format("first input: %s\n", inputName);
        int outputs = wrapper.cogFunctionOutputCount(wrapper.ComputeFunctionPtr);
        System.out.format("output count: %d\n", outputs);
        String outputName = wrapper.cogFunctionOutputName(wrapper.ComputeFunctionPtr, 0);
        System.out.format("first output: %s\n", outputName);

        // run the model

        // Float.Size is bits, not bytes
        ByteBuffer inBuf = ByteBuffer.allocate(Float.SIZE / 8);
        // C uses little endian for floats
        inBuf.order(ByteOrder.LITTLE_ENDIAN);
        ByteBuffer outBuf = ByteBuffer.allocate(Float.SIZE / 8);
        outBuf.order(ByteOrder.LITTLE_ENDIAN);

        float startVal = -2.5f;
        float endVal = +2.5f;
        float step = 0.5f;
        float floatVal = 0.0f;

        // Loop over a number of test cases...
        for (floatVal = startVal; floatVal <= endVal; floatVal += step) {
            inBuf.putFloat(0, floatVal);
//            System.out.println(Arrays.toString(inBuf.array()));
//            for (int i = 0; i < inputBytes.length; i++)
//                System.out.format("bytes: %x\n", inputBytes[i]);

            // Run the model on this one input
            wrapper.cogFunctionEvaluate(wrapper.ComputeFunctionPtr, inBuf.array(), outBuf.array());
            // Check the model successfully incremented the input
            float expectedOutVal = floatVal + 1.0f;
            if (outBuf.getFloat(0) != expectedOutVal) {
                System.out.format("Error*** Expecting actuator output value % .3f, found instead % .3f.\n",
                        expectedOutVal, outBuf.getFloat(0));
            }
        }



        // shutdown
        int err = wrapper.cogFunctionShutdown(wrapper.ComputeFunctionPtr);
        System.out.format("CogErr: %d\n", err);

        // now try the adder model
        System.out.format("***Loading file adder.h5.....\n");
        wrapper.ComputeFunctionPtr = wrapper.cogFunctionCreateComputeFunction("adder.h5", 0);
        name = wrapper.cogFunctionName(wrapper.ComputeFunctionPtr);
        System.out.format("Computegraph name: %s\n", name);
        System.out.format("thread count: %d\n", threadCount);

        // check inputs and outputs
        inputs = wrapper.cogFunctionInputCount(wrapper.ComputeFunctionPtr);
        System.out.format("input count: %d\n", inputs);
        for (int i = 0; i < inputs; i++) {
            inputName = wrapper.cogFunctionInputName(wrapper.ComputeFunctionPtr, i);
            System.out.format("input %d: %s\n", i, inputName);
        }
        outputs = wrapper.cogFunctionOutputCount(wrapper.ComputeFunctionPtr);
        System.out.format("output count: %d\n", outputs);
        outputName = wrapper.cogFunctionOutputName(wrapper.ComputeFunctionPtr, 0);
        System.out.format("first output: %s\n", outputName);

        // input field
        long fieldTypePtr = wrapper.cogFunctionInputFieldType(wrapper.ComputeFunctionPtr,0);
        int row = wrapper.cogFieldTypeRows(fieldTypePtr);
        int col = wrapper.cogFieldTypeColumns(fieldTypePtr);
        int bytes = wrapper.cogFieldTypeBufferSizeBytes(fieldTypePtr);
        int dimensions = wrapper.cogFieldTypeDimensions(fieldTypePtr);
        int order = wrapper.cogFieldTypeTensorOrder(fieldTypePtr);
        int type = wrapper.cogFieldTypeElementType(fieldTypePtr);
        System.out.format("first input field: rows %d cols %d bytes %d dimensions %d order %d element type %d\n",
                row, col, bytes, dimensions, order, type);
        int layers = 0, tensorRow = 0, tensorCol = 0;
        // this model has tensor order 0, and dimensions 2, so all the following will generate exceptions
//        layers = wrapper.cogFieldTypeLayers(fieldTypePtr);
//        tensorRow = wrapper.cogFieldTypeTensorRows(fieldTypePtr);
//        tensorCol = wrapper.cogFieldTypeTensorColumns(fieldTypePtr);
        System.out.format("first input field: layers %d tensorRow %d tensorCol %d\n",
                layers, tensorRow, tensorCol);

        // outputfield
        fieldTypePtr = wrapper.cogFunctionOutputFieldType(wrapper.ComputeFunctionPtr, 0);
        row = wrapper.cogFieldTypeRows(fieldTypePtr);
        col = wrapper.cogFieldTypeColumns(fieldTypePtr);
        bytes = wrapper.cogFieldTypeBufferSizeBytes(fieldTypePtr);
        System.out.format("first output field -  rows %d cols %d bytes %d\n", row, col, bytes);

        // now run it
        int Rows = 3;
        int Columns = 4;
        int numElems = Rows * Columns;

        // Bundle the two input arrays as a single flattened array argument to the compute function.
//        float[] inBufs = new float[numElems * 2];
        int floatSize = Float.SIZE / 8;
        ByteBuffer inBuf2 = ByteBuffer.allocate(floatSize * numElems * 2 );
        inBuf2.order(ByteOrder.LITTLE_ENDIAN);
        ByteBuffer outBuf2 = ByteBuffer.allocate(floatSize * numElems);
        outBuf2.order(ByteOrder.LITTLE_ENDIAN);

        // Check that the model can add the two fields
        float startVal0 = -2.5f;
        float endVal0 = +2.5f;
        float step0 = 0.5f;
        float startVal1 = 3.875f;
        float step1 = -0.25f;

        float floatVal0 = 0.0f;
        float floatVal1 = 0.0f;

        // Loop over a number of test cases...
        int input2Offset = numElems  * floatSize;
        for (floatVal0 = startVal0, floatVal1 = startVal1; floatVal0 <= endVal0; floatVal0 += step0, floatVal1 += step1) {
            int i;
            // Initialize the two inputs
            for (i = 0; i < numElems; i++) {
                inBuf2.putFloat(i*floatSize, floatVal0 + i);
                inBuf2.putFloat(i*floatSize +input2Offset, floatVal1 + i);
            }
//            System.out.println(Arrays.toString(inBuf2.array()));
            // Run the model on this one test case
            wrapper.cogFunctionEvaluate(wrapper.ComputeFunctionPtr, inBuf2.array(), outBuf2.array());
            // Check the model successfully added the two inputs
            for (i = 0; i < numElems; i++) {
                float expectedOutVal = (floatVal0 + i) + (floatVal1 + i);
                if (outBuf2.getFloat(i*floatSize) != expectedOutVal)
                    System.out.format("Error*** Expecting actuator output value % .3f, found instead % .3f.\n",
                            expectedOutVal, outBuf2.getFloat(i*floatSize));
            }
        }

        // now try as 2D arrays
        System.out.format("Try using 2D arrays \n");
        ByteBuffer inVal0 = ByteBuffer.allocate(floatSize * numElems);
        inVal0.order(ByteOrder.LITTLE_ENDIAN);
        ByteBuffer inVal1 = ByteBuffer.allocate(floatSize * numElems);
        inVal1.order(ByteOrder.LITTLE_ENDIAN);
        ByteBuffer outVal = ByteBuffer.allocate(floatSize * numElems);
        outVal.order(ByteOrder.LITTLE_ENDIAN);

        // Check that the model can add the two fields
        startVal0 = -2.5f;
        endVal0 = +2.5f;
        step0 = 0.5f;
        startVal1 = 3.875f;
        step1 = -0.25f;

        byte[][] inBufs = new byte[2][];
        inBufs[0] = inVal0.array();
        inBufs[1] = inVal1.array();
        byte[][] outBufs = new byte[1][];
        outBufs[0] = outVal.array();

        // Loop over a number of test cases...
        for (floatVal0 = startVal0, floatVal1 = startVal1; floatVal0 <= endVal0; floatVal0 += step0, floatVal1 += step1) {
            int i;
            // Initialize the two inputs
            for (i = 0; i < numElems; i++) {
                inVal0.putFloat(i*floatSize, floatVal0 + i);
                inVal1.putFloat(i*floatSize, floatVal1 + i);
            }
//            System.out.println(Arrays.toString(inBufs[0]));
//            System.out.println(Arrays.toString(inBufs[1]));
            // Run the model on this one test case
            wrapper.cogFunctionEvaluate2D(wrapper.ComputeFunctionPtr, inBufs, outBufs);
            // Check the model successfully added the two inputs
            for (i = 0; i < numElems; i++) {
                float expectedOutVal = (floatVal0 + i) + (floatVal1 + i);
                if (outVal.getFloat(i*floatSize) != expectedOutVal)
                    System.out.format("Error*** Expecting actuator output value %.3f, found instead %.3f.\n",
                            expectedOutVal, outVal.getFloat(i*floatSize));
            }
        }

        // now try the counter model
        System.out.format("***Loading file CounterGraphOf2DScalarFields.h5.....\n");
        wrapper.ComputeFunctionPtr = wrapper.cogFunctionCreateComputeFunction("CounterGraphOf2DScalarFields.h5", 0);
        name = wrapper.cogFunctionName(wrapper.ComputeFunctionPtr);
        System.out.format("Computegraph name: %s\n", name);

        // this model has no inputs or outputs
        inputs = wrapper.cogFunctionInputCount(wrapper.ComputeFunctionPtr);
        System.out.format("input count: %d\n", inputs);
        outputs = wrapper.cogFunctionOutputCount(wrapper.ComputeFunctionPtr);
        System.out.format("output count: %d\n", outputs);

        // get information about the tensor field
        long tensorFieldTypePtr = wrapper.cogTensorFieldType(wrapper.ComputeFunctionPtr, "counter");
        int rows = wrapper.cogFieldTypeRows(tensorFieldTypePtr);
        int cols = wrapper.cogFieldTypeColumns(tensorFieldTypePtr);
        bytes = wrapper.cogFieldTypeBufferSizeBytes(tensorFieldTypePtr);
        dimensions = wrapper.cogFieldTypeDimensions(tensorFieldTypePtr);
        order = wrapper.cogFieldTypeTensorOrder(tensorFieldTypePtr);
        type = wrapper.cogFieldTypeElementType(tensorFieldTypePtr);
        System.out.format("counter tensor field: rows %d cols %d bytes %d dimensions %d order %d element type %d\n",
                row, col, bytes, dimensions, order, type);

        // look at the data inside the model
        String targetFieldName = "counter";  // The field name in the HDF5 file.
        // Destination buffer for data coming out of the graph
        float dataBuffer[] = new float[rows * cols];

        byte[][] emptyIn = new byte[0][0];
        byte[][] emptyOut = new byte[0][0];

        for (int s = 0; s < 2; s++) {
            // now step it - first step does the reset
            wrapper.cogFunctionEvaluate2D(wrapper.ComputeFunctionPtr, emptyIn, emptyOut);
            wrapper.cogReadFieldByName(wrapper.ComputeFunctionPtr, targetFieldName, dataBuffer);
            System.out.format("counter at step %d:\n", s);
            for (int r = 0; r < rows; ++r) {
                for (int c = 0; c < cols; ++c) {
                    System.out.format("%.3f ", dataBuffer[r * cols + c]);
                }
                System.out.format("\n");
            }
        }

        // shutdown
        System.out.format("Done. Shutting down... \n");
        wrapper.cogFunctionShutdownAll();
    }

}
