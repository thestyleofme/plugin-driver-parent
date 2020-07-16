package com.github.codingdebugallday.driver.common.app.service.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import com.github.codingdebugallday.driver.common.app.service.PluginMinioService;
import com.github.codingdebugallday.driver.common.infra.constants.CommonConstant;
import com.github.codingdebugallday.driver.common.infra.exceptions.DriverException;
import com.github.codingdebugallday.driver.common.infra.utils.DriverCommonUtil;
import io.minio.*;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.FileUtils;
import org.springframework.util.Assert;
import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/14 16:00
 * @since 1.0
 */
@Slf4j
public class PluginMinioServiceImpl implements PluginMinioService {

    private final MinioClient pluginMinioClient;

    public PluginMinioServiceImpl(MinioClient pluginMinioClient) {
        Assert.notNull(pluginMinioClient,
                "You have used Minio to store the plugin, please configure the properties of minio");
        this.pluginMinioClient = pluginMinioClient;
    }

    @Override
    public String createBucketAndUploadObject(String bucketName, MultipartFile multipartFile, String objectName) {
        // 上传到minio
        File tmp = null;
        try {
            tmp = DriverCommonUtil.multiPartFileToFile(multipartFile);
            return createBucketAndUploadObject(bucketName, tmp, objectName);
        } finally {
            try {
                if (tmp != null) {
                    FileUtils.forceDelete(tmp);
                }
            } catch (IOException e) {
                // ignore
            }
        }
    }

    @Override
    public String createBucketAndUploadObject(String bucketName, File file, String objectName) {
        try {
            if (!pluginMinioClient.bucketExists(BucketExistsArgs.builder()
                    .bucket(CommonConstant.PLUGIN_MINIO_BUCKET).build())) {
                pluginMinioClient.makeBucket(MakeBucketArgs.builder()
                        .bucket(CommonConstant.PLUGIN_MINIO_BUCKET).build());
            }
            pluginMinioClient.uploadObject(UploadObjectArgs.builder()
                    .bucket(CommonConstant.PLUGIN_MINIO_BUCKET)
                    .object(objectName)
                    .filename(file.getName())
                    .build());
            return pluginMinioClient.getObjectUrl(CommonConstant.PLUGIN_MINIO_BUCKET, objectName);
        } catch (Exception e) {
            log.error("createBucketAndUploadObject error");
            throw new DriverException("createBucketAndUploadObject error", e);
        }
    }

    @Override
    public InputStream getObject(String bucketName, String objectName) {
        try {
            return pluginMinioClient.getObject(GetObjectArgs.builder()
                    .bucket(bucketName).object(objectName).build());
        } catch (Exception e) {
            log.error("getObject error");
            throw new DriverException("getObject error", e);
        }
    }

    @Override
    public void removeObject(String bucketName, String objectName) {
        try {
            pluginMinioClient.removeObject(RemoveObjectArgs.builder()
                    .bucket(bucketName).object(objectName).build());
        } catch (Exception e) {
            log.error("removeObject error");
            throw new DriverException("removeObject error", e);
        }
    }
}
