package com.github.thestyleofme.plugin.core.infra.utils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Objects;

import com.github.thestyleofme.plugin.framework.exceptions.PluginException;
import org.springframework.util.Assert;
import org.springframework.util.FileCopyUtils;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/14 17:31
 * @since 1.0.0
 */
public class PluginCommonUtil {

    private PluginCommonUtil() {
        throw new IllegalStateException("util class");
    }

    public static File multiPartFileToFile(MultipartFile multipartFile) {
        Assert.notNull(multipartFile, "multipartFile must not be null");
        try (InputStream inputStream = multipartFile.getInputStream()) {
            File toFile = new File(Objects.requireNonNull(multipartFile.getOriginalFilename()));
            byte[] bytes = FileCopyUtils.copyToByteArray(inputStream);
            FileCopyUtils.copy(bytes, toFile);
            return toFile;
        } catch (IOException e) {
            throw new PluginException("multiPartFileToFile error", e);
        }
    }
}
