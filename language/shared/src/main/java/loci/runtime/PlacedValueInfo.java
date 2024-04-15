package loci.runtime;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface PlacedValueInfo {
  String signature();
  String arguments();
  String result();
}
