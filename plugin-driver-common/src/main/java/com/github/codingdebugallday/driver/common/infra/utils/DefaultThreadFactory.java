package com.github.codingdebugallday.driver.common.infra.utils;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * <p>
 * 线程池
 * </p>
 *
 * @author JupiterMouse 2020/07/14
 * @since 1.0.0
 */
public class DefaultThreadFactory implements ThreadFactory {

    /**
     * 线程组
     */
    private final ThreadGroup group;

    /**
     * 线程编号
     */
    private final AtomicInteger threadNumber = new AtomicInteger(1);

    /**
     * 线程池名称前缀
     */
    private final String namePrefix;

    public DefaultThreadFactory(String namePrefix) {
        SecurityManager manager = System.getSecurityManager();
        this.group = (manager != null) ? manager.getThreadGroup() :
                Thread.currentThread().getThreadGroup();
        this.namePrefix = namePrefix + "-";
    }

    @SuppressWarnings("NullableProblems")
    @Override
    public Thread newThread(Runnable runnable) {
        String name = namePrefix + threadNumber.getAndIncrement();
        Thread thread = new Thread(group, runnable, name, 0);
        if (thread.isDaemon()) {
            thread.setDaemon(false);
        }
        if (thread.getPriority() != Thread.NORM_PRIORITY) {
            thread.setPriority(Thread.NORM_PRIORITY);
        }
        return thread;
    }
}
