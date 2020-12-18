package com.github.thestyleofme.driver.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
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

    private static final Logger logger = LoggerFactory.getLogger(DriverApplication.class);

    public static void main(String[] args) {
        try {
            SpringApplication.run(DriverApplication.class, args);
        } catch (Exception e) {
            logger.error("start error",e);
        }
    }
}
