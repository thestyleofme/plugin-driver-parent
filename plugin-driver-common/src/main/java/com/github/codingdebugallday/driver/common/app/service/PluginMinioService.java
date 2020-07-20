package com.github.codingdebugallday.driver.common.app.service;

import java.io.File;
import java.io.InputStream;

import org.springframework.web.multipart.MultipartFile;

/**
 * <p>
 * description
 * </p>
 *
 * @author isaac 2020/7/14 15:54
 * @since 1.0.0
 */
public interface PluginMinioService {

    /**
     * 创建桶并上传文件到minio
     *
     * @param bucketName    bucket
     * @param multipartFile MultipartFile
     * @param objectName    object
     * @return 返回文件上传到minio后的地址
     */
    String createBucketAndUploadObject(String bucketName,
                                       MultipartFile multipartFile,
                                       String objectName);

    /**
     * 创建桶并上传文件到minio
     *
     * @param bucketName bucket
     * @param file       File
     * @param objectName object
     * @return 返回文件上传到minio后的地址
     */
    String createBucketAndUploadObject(String bucketName,
                                       File file,
                                       String objectName);

    /**
     * 获取minio的object
     *
     * @param bucketName bucket
     * @param objectName object
     * @return Returned InputStream must be closed after use to release network resources.
     */
    InputStream getObject(String bucketName, String objectName);

    /**
     * 删除minio的object
     *
     * @param bucketName bucket
     * @param objectName object
     */
    void removeObject(String bucketName, String objectName);

}
