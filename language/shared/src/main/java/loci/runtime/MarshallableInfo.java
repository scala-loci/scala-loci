package loci.runtime;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface MarshallableInfo {
  int signature();
  String base();
  String result();
  String proxy();
}
