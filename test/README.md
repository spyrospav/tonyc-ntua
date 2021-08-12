## Test cases format
In order to run the test scripts the `.tony` files should have the following comment at the start :
```
<*
 * Test case: example.tony
 * Result: Pass/Fail
 *>
```
where `example.tony` is the name of the file and the result should be `Pass` if the file shall pass the semantic analysis and `Fail` otherwise. 

* `test.sh` performs semantic testing
* `test_compile.sh` checks whether the semantically correct programs compile succesfully

If some program requires user input to run, then you must manually edit the specification comment at **line 4** as follows :

```
<*
 * Test case: example.tony
 * Result: Pass/Fail
 * Input
 *>
```
