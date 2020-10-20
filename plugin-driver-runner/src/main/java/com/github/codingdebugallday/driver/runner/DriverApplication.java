package com.github.thestyleofme.driver.runner;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/6/29 16:10
 * @since 1.0.0
 */
@SpringBootApplication
public class DriverApplication {

    public static void main(String[] args) {
        try {
            SpringApplication.run(DriverApplication.class, args);
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
}
