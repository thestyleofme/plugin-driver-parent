package com.github.codingdebugallday.driver.runner;

import org.mybatis.spring.annotation.MapperScan;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/6/29 16:10
 * @since 1.0
 */
@SpringBootApplication
@MapperScan({
        "com.github.codingdebugallday.driver.**.mapper"
})
public class DriverApplication {

    public static void main(String[] args) {
        try {
            SpringApplication.run(DriverApplication.class, args);
        } catch (Throwable e) {
            e.printStackTrace();
        }
    }
}
