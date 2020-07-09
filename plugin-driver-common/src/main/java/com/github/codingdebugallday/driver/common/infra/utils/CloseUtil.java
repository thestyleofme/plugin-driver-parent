package com.github.codingdebugallday.driver.common.infra.utils;

/**
 * <p>
 * 关闭流
 * </p>
 *
 * @author JupiterMouse 2020/07/08
 * @since 1.0
 */
public final class CloseUtil {

    private CloseUtil() {
        throw new IllegalStateException("Utils");
    }

    public static void close(AutoCloseable... closeables) {
        for (AutoCloseable closeable : closeables) {
            if (closeable != null) {
                try {
                    closeable.close();
                } catch (Exception e) {
                    // ignore
                }
            }
        }
    }

}
