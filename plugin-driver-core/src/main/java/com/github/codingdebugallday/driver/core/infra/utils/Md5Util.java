package com.github.codingdebugallday.driver.core.infra.utils;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;

import com.github.codingdebugallday.driver.core.infra.exceptions.DriverException;
import lombok.extern.slf4j.Slf4j;
import org.springframework.util.DigestUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/14 16:23
 * @since 1.0.0
 */
@Slf4j
public class Md5Util {

    private Md5Util() {
        throw new IllegalStateException();
    }

    public static String md5DigestAsHex(MultipartFile multipartFile) {
        try (InputStream inputStream = new BufferedInputStream(multipartFile.getInputStream())) {
            return DigestUtils.md5DigestAsHex(inputStream);
        } catch (IOException e) {
            log.error("md5DigestAsHex error");
            throw new DriverException("md5DigestAsHex error", e);
        }
    }
}
